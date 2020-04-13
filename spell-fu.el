;;; spell-fu.el --- Fast & light spelling highlighter -*- lexical-binding: t -*-

;; Copyright (C) 2020  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://gitlab.com/ideasman42/emacs-spell-fu-mode
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package checks the spelling of on-screen text.
;;

;;; Usage

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'spell-fu)
;;   (global-spell-fu-mode-mode)
;;
;; Or with `use-package':
;;
;;   (use-package spell-fu)
;;   (global-spell-fu-mode-mode)
;;
;; If you prefer to enable this per-mode, you may do so using
;; mode hooks instead of calling `global-spell-fu-mode-mode'.
;; The following example enables this for org-mode:
;;
;;   (add-hook 'org-mode-hook
;;     (lambda ()
;;       (setq spell-fu-faces-exclude '(org-meta-line))
;;       (spell-fu-mode)))
;;

;;; Code:


;; ---------------------------------------------------------------------------
;; Require Dependencies

;; For `face-list-p'.
(require 'faces)
;; For variables we read `ispell-personal-dictionary' local dictionary, etc.
(require 'ispell)


;; ---------------------------------------------------------------------------
;; Custom Variables

(defcustom spell-fu-directory (locate-user-emacs-file "spell-fu" ".emacs-spell-fu")
  "The directory to store undo data."
  :group 'spell-fu
  :type 'string)

(defcustom spell-fu-idle-delay 0.25
  "Idle time to wait before highlighting.
Set to 0.0 to highlight immediately (as part of syntax highlighting)."
  :group 'spell-fu
  :type 'float)

(defface spell-fu-incorrect-face
  '((t (:underline (:color "red" :style wave))))
  "Face for incorrect spelling."
  :group 'spell-fu)

;; See '-' as a word boundary \b, so 'full-screen' is detected as two words.
(defvar-local spell-fu-syntax-table
  (let ((table (copy-syntax-table (standard-syntax-table))))
    (modify-syntax-entry ?- "-" table)
    table)
  "The syntax table to use when scanning words.")

(defvar-local spell-fu-word-regexp "\\b\\([[:alpha:]][[:alpha:]']*\\)\\b"
  "The regular expression used to scan for words to check (used by `spell-fu-check-range').")

(defvar-local spell-fu-faces-include nil
  "List of faces to check or nil to include all (used by `spell-fu-check-range').")

(defvar-local spell-fu-faces-exclude nil
  "List of faces to check or nil to exclude none (used by `spell-fu-check-range').")


(defvar-local spell-fu-check-range 'spell-fu-check-range-default
  "Function that takes a beginning and end points to check for the current buffer.

Users may want to write their own functions to have more control
over which words are being checked.

Notes:

- The ranges passed in a guaranteed to be on line boundaries.
- Calling `spell-fu-check-word' on each word.

- You may explicitly mark a range as incorrect using
  `spell-fu-mark-incorrect' which takes the range to mark as arguments.")


;; ---------------------------------------------------------------------------
;; Internal Variables


;; Use to ensure the cache is not from a previous release.
;; Only ever increase.
(defconst spell-fu--cache-version "0.1")

;; List of language, dictionary mappings.
(defvar spell-fu--cache-table-alist nil)

;; Buffer local dictionary.
;; Note that this is typically the same dictionary shared across all buffers.
(defvar-local spell-fu--cache-table nil)


;; ---------------------------------------------------------------------------
;; Dictionary Utility Functions

(defun spell-fu--dictionary ()
  "Access the current dictionary."
  (or ispell-local-dictionary ispell-dictionary "default"))

(defun spell-fu--cache-file (dict)
  "Return the location of the cache file with dictionary DICT."
  (expand-file-name (format "words_%s.el.data" dict) spell-fu-directory))

(defun spell-fu--words-file (dict)
  "Return the location of the word-list with dictionary DICT."
  (expand-file-name (format "words_%s.txt" dict) spell-fu-directory))


;; ---------------------------------------------------------------------------
;; Generic Utility Functions
;;
;; Helpers, not directly related to checking spelling.
;;

(defmacro spell-fu--with-advice (fn-orig where fn-advice &rest body)
  "Execute BODY with WHERE advice on FN-ORIG temporarily enabled."
  `
  (let ((fn-advice-var ,fn-advice))
    (unwind-protect
      (progn
        (advice-add ,fn-orig ,where fn-advice-var)
        ,@body)
      (advice-remove ,fn-orig fn-advice-var))))

(defmacro spell-fu--with-message-prefix (prefix &rest body)
  "Add text before the message output.
Argument PREFIX is the text to add at the start of the message.
Optional argument BODY runs with the message prefix."
  (declare (indent 1))
  `
  (spell-fu--with-advice 'message
    :around
    (lambda (fn-orig arg &rest args)
      (apply fn-orig (append (list (concat "%s" arg)) (list ,prefix) args)))
    ,@body))

(defmacro spell-fu--with-add-hook-depth-override (depth-override &rest body)
  "Support overriding the depth of a hook added by an indirect call.
Argument DEPTH-OVERRIDE the depth value to call `add-hook' with.
Optional argument BODY runs with the depth override."
  (declare (indent 1))
  `
  (spell-fu--with-advice 'add-hook
    :around
    (lambda (fn-orig hook function &optional _depth local)
      (funcall fn-orig hook function ,depth-override local))
    ,@body))

(defmacro spell-fu--setq-expand-range-to-line-boundaries (point-start point-end)
  "Set POINT-START the the line beginning, POINT-END to the line end."
  (declare (indent 1))
  ;; Ignore field boundaries.
  (let ((inhibit-field-text-motion t))
    `
    (save-excursion
      ;; Extend the ranges to line start/end.
      (goto-char ,point-end)
      (setq ,point-end (line-end-position))
      (goto-char ,point-start)
      (setq ,point-start (line-beginning-position)))))

(defun spell-fu--buffer-as-line-list (buffer lines)
  "Add lines from BUFFER to LINES, returning the updated LINES."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
        (forward-line 1))))
  lines)

(defun spell-fu--removed-changed-overlay (overlay after _beg _end &optional _len)
  "Hook for removing OVERLAY which is being edited.
Argument AFTER, ignore when true."
  (unless after
    (delete-overlay overlay)))

(defun spell-fu--faces-at-point (pos)
  "Add the named faces that the `read-face-name' or `face' property use.
Argument POS return faces at this point."
  (let
    ( ;; List of faces to return.
      (faces nil)
      (faceprop (or (get-char-property pos 'read-face-name) (get-char-property pos 'face))))
    (cond
      ((facep faceprop)
        (push faceprop faces))
      ((face-list-p faceprop)
        (dolist (face faceprop)
          (when (facep face)
            (push face faces)))))
    faces))

(defun spell-fu--file-is-older-list (file-test file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (catch 'result
    (let ((file-test-time (file-attribute-modification-time (file-attributes file-test))))
      (dolist (file-new file-list)
        (when
          (time-less-p
            file-test-time
            (file-attribute-modification-time (file-attributes file-new)))
          (throw 'result t)))
      nil)))

(defun spell-fu--file-is-older (file-test &rest file-list)
  "Return t when FILE-TEST is older than any files in FILE-LIST."
  (spell-fu--file-is-older-list file-test file-list))


;; ---------------------------------------------------------------------------
;; Word List Generation

(defun spell-fu--word-list-ensure (words-file dict)
  "Ensure the word list is generated with dictionary DICT.
Argument WORDS-FILE the file to write the word list into."
  (let*
    (
      (personal-words-file ispell-personal-dictionary)
      (has-words-file (file-exists-p words-file))
      (has-dict-personal (and personal-words-file (file-exists-p personal-words-file)))
      (is-dict-outdated
        (and
          has-words-file
          has-dict-personal
          (spell-fu--file-is-older words-file personal-words-file))))

    (when (or (not has-words-file) is-dict-outdated)

      (spell-fu--with-message-prefix "Spell-fu generating words: "
        (message "%S" (file-name-nondirectory words-file))

        ;; Build a word list, sorted case insensitive.
        (let ((word-list nil))

          ;; Optional: insert personal dictionary, stripping header and inserting a newline.
          (with-temp-buffer
            (when has-dict-personal
              (insert-file-contents personal-words-file)
              (goto-char (point-min))
              (when (looking-at "personal_ws\-")
                (delete-region (line-beginning-position) (1+ (line-end-position))))
              (goto-char (point-max))
              (unless (eq ?\n (char-after))
                (insert "\n")))

            (setq word-list (spell-fu--buffer-as-line-list (current-buffer) word-list)))

          ;; Insert dictionary from aspell.
          (with-temp-buffer
            (let
              ( ;; Use the pre-configured aspell binary, or call aspell directly.
                (aspell-bin
                  (or (and ispell-really-aspell ispell-program-name) (executable-find "aspell"))))

              (cond
                ((string-equal dict "default")
                  (call-process aspell-bin nil t nil "dump" "master"))
                (t
                  (call-process aspell-bin nil t nil "-d" dict "dump" "master"))))

            (setq word-list (spell-fu--buffer-as-line-list (current-buffer) word-list)))

          ;; Case insensitive sort is important if this is used for `ispell-complete-word-dict'.
          ;; Which is a handy double-use for this file.
          (let ((word-list-ncase nil))
            (dolist (word word-list)
              (push (cons (downcase word) word) word-list-ncase))

            ;; Sort by the lowercase word.
            (setq word-list-ncase
              (sort word-list-ncase (lambda (a b) (string-lessp (car a) (car b)))))

            ;; Write to 'words-file'.
            (with-temp-buffer
              (dolist (line-cons word-list-ncase)
                (insert (cdr line-cons) "\n"))
              (write-region nil nil words-file nil 0))))))))


;; ---------------------------------------------------------------------------
;; Word List Cache

(defun spell-fu--cache-from-word-list-impl (words-file cache-file)
  "Create CACHE-FILE from WORDS-FILE.

The resulting cache is returned as a minor optimization for first-time loading,
where we need to create this data in order to write it,
save some time by not spending time reading it back."
  (message "%S" (file-name-nondirectory cache-file))
  (let
    ( ;; The header, an associative list of items.
      (cache-header (list (cons "version" spell-fu--cache-version)))
      (word-table nil))

    (with-temp-buffer
      (insert-file-contents-literally words-file)
      (setq word-table (make-hash-table :test #'equal :size (count-lines (point-min) (point-max))))
      (while (not (eobp))
        (let ((l (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          ;; Value of 't' is just for simplicity, it's no used except for check the item exists.
          (puthash (downcase l) t word-table)
          (forward-line 1))))

    ;; Write write it to a file.
    (with-temp-buffer
      (prin1 cache-header (current-buffer))
      (prin1 word-table (current-buffer))
      (write-region nil nil cache-file nil 0))

    ;; Return the resulting word table.
    word-table))

(defun spell-fu--cache-from-word-list (words-file cache-file)
  "Create CACHE-FILE from WORDS-FILE."
  (spell-fu--with-message-prefix "Spell-fu generating cache: "
    (condition-case err
      (spell-fu--cache-from-word-list-impl words-file cache-file)
      (error
        ;; Should be rare: if the file is corrupt or cannot be read for any reason.
        (progn
          (message "failed, %s" (error-message-string err))
          nil)))))


(defun spell-fu--cache-words-load-impl (cache-file)
  "Return the Lisp content from reading CACHE-FILE.

On failure of any kind, return nil, the caller will need to regenerate the cache."
  (with-temp-buffer
    (insert-file-contents-literally cache-file)
    (goto-char (point-min))

    ;; Check header.
    (let ((cache-header (read (current-buffer))))
      (unless (listp cache-header)
        (error "Expected cache-header to be list, not %S" (type-of cache-header)))

      (let ((version (assoc-default "version" cache-header)))
        (unless (string-equal version spell-fu--cache-version)
          (error "Require cache version %S, not %S" spell-fu--cache-version version))))

    ;; Read the contents.
    (let ((word-table (read (current-buffer))))
      (unless (hash-table-p word-table)
        (error "Expected cache to contain a hash-table, not %S" (type-of word-table)))
      word-table)))

(defun spell-fu--cache-words-load (cache-file)
  "Return the Lisp content from reading CACHE-FILE."
  (spell-fu--with-message-prefix "Spell-fu reading cache: "
    (condition-case err
      (spell-fu--cache-words-load-impl cache-file)
      (error
        ;; Should be rare: if the file is corrupt or cannot be read for any reason.
        (progn
          (message "failed, %s" (error-message-string err))
          nil)))))

;; ---------------------------------------------------------------------------
;; Word List Initialization
;;
;; Top level function, called when enabling the mode.

(defun spell-fu--init-dictionary (dict)
  "Setup the dictionary, initializing new files as necessary with dictionary DICT."

  ;; Ensure our path exists.
  (unless (file-directory-p spell-fu-directory)
    (make-directory spell-fu-directory))

  (let
    ( ;; Get the paths of both files, ensure the cache file is newer,
      ;; otherwise regenerate it.
      (words-file (spell-fu--words-file dict))
      (cache-file (spell-fu--cache-file dict)))

    (spell-fu--word-list-ensure words-file dict)


    ;; Use previously loaded dictionary from language 'dict' where possible.
    (setq spell-fu--cache-table (assoc-default dict spell-fu--cache-table-alist))

    ;; Not loaded yet, initialize it.
    (unless spell-fu--cache-table
      ;; Load cache or create it, creating it returns the cache
      ;; to avoid some slow-down on first load.
      (setq spell-fu--cache-table
        (or
          (and
            (file-exists-p cache-file)
            (not (spell-fu--file-is-older cache-file words-file))
            (spell-fu--cache-words-load cache-file))
          (spell-fu--cache-from-word-list words-file cache-file)))

      ;; Add to to `spell-fu--cache-table-alist' for reuse on next load.
      (push '(dict . spell-fu--cache-table) spell-fu--cache-table-alist))))


;; ---------------------------------------------------------------------------
;; Shared Functions

(defun spell-fu--remove-overlays (&optional point-start point-end)
  "Remove symbol `spell-fu-mode' overlays from current buffer.
If optional arguments POINT-START and POINT-END exist remove overlays from
range POINT-START to POINT-END. Otherwise remove all overlays."
  (remove-overlays point-start point-end 'spell-fu-mode t))

(defun spell-fu-mark-incorrect (point-start point-end)
  "Mark the text from POINT-START to POINT-END with the default incorrect spelling overlay."
  (let ((item-ov (make-overlay point-start point-end)))
    (overlay-put item-ov 'spell-fu-mode t)
    (overlay-put item-ov 'face 'spell-fu-incorrect-face)
    (overlay-put item-ov 'modification-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'insert-in-front-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'insert-behind-hooks 'spell-fu--removed-changed-overlay)
    (overlay-put item-ov 'evaporate t)
    item-ov))

(defun spell-fu-check-word (point-start point-end word)
  "Run the spell checker on a word.

Marking the spelling as incorrect using `spell-fu-incorrect-face' on failure.
Argument POINT-START the beginning position of WORD.
Argument POINT-END the end position of WORD."
  (unless (gethash (downcase word) spell-fu--cache-table nil)
    ;; Ignore all uppercase words.
    (unless (equal word (upcase word))
      (spell-fu-mark-incorrect point-start point-end))))


;; ---------------------------------------------------------------------------
;; Range Checking Commands
;;
;; These functions are value values for the `spell-fu-check-range' buffer local variable.
;;
;; Note that the callers of these function extends the range to line delimiters,
;; to ensure there no chance of the points being in the middle of a word.
;;

(defun spell-fu--check-faces-at-point (pos faces-include faces-exclude)
  "Check if the position POS has faces that match the include/exclude arguments.

Argument FACES-INCLUDE faces to check POS includes or ignored when nil.
Argument FACES-EXCLUDE faces to check POS excludes or ignored when nil."
  (let
    (
      (result (null faces-include))
      (faces-at-pos (spell-fu--faces-at-point pos))
      (face nil))
    (while (setq face (pop faces-at-pos))
      (when (memq face faces-exclude)
        (setq faces-at-pos nil)
        (setq result nil))
      (when (and (null result) (memq face faces-include))
        (setq result t)))
    result))


(defun spell-fu--check-range-with-faces (point-start point-end)
  "Check spelling for POINT-START & POINT-END, checking text matching face rules."
  (spell-fu--remove-overlays point-start point-end)
  (with-syntax-table spell-fu-syntax-table
    (save-match-data
      (save-excursion
        (goto-char point-start)
        (while (re-search-forward spell-fu-word-regexp point-end t)
          (let
            (
              (word-start (match-beginning 0))
              (word-end (match-end 0)))
            (when
              (spell-fu--check-faces-at-point
                word-start
                spell-fu-faces-include
                spell-fu-faces-exclude)
              (spell-fu-check-word word-start word-end (match-string-no-properties 0)))))))))

(defun spell-fu--check-range-without-faces (point-start point-end)
  "Check spelling for POINT-START & POINT-END, checking all text."
  (spell-fu--remove-overlays point-start point-end)
  (with-syntax-table spell-fu-syntax-table
    (save-match-data
      (save-excursion
        (goto-char point-start)
        (while (re-search-forward spell-fu-word-regexp point-end t)
          (let
            (
              (word-start (match-beginning 0))
              (word-end (match-end 0)))
            (spell-fu-check-word word-start word-end (match-string-no-properties 0))))))))

(defun spell-fu-check-range-default (point-start point-end)
  "Check spelling POINT-START & POINT-END, checking comments and strings."
  (if (or spell-fu-faces-include spell-fu-faces-exclude)
    (spell-fu--check-range-with-faces point-start point-end)
    (spell-fu--check-range-without-faces point-start point-end)))


;; ---------------------------------------------------------------------------
;; Immediate Style (spell-fu-idle-delay zero or lower)

(defun spell-fu--font-lock-fontify-region (point-start point-end)
  "Update spelling for POINT-START & POINT-END to the queue, checking all text."
  (spell-fu--setq-expand-range-to-line-boundaries
    ;; Warning these values are set in place.
    point-start point-end)
  (funcall spell-fu-check-range point-start point-end))

(defun spell-fu--immediate-enable ()
  "Enable immediate spell checking."

  ;; It's important this is added with a depth of 100,
  ;; because we want the font faces (comments, string etc) to be set so
  ;; the spell checker can read these values which may include/exclude words.
  (spell-fu--with-add-hook-depth-override 100
    (jit-lock-register #'spell-fu--font-lock-fontify-region)))

(defun spell-fu--immediate-disable ()
  "Disable immediate spell checking."
  (jit-lock-unregister #'spell-fu--font-lock-fontify-region)
  (spell-fu--remove-overlays))


;; ---------------------------------------------------------------------------
;; Timer Style (spell-fu-idle-delay over zero)

(defvar spell-fu--idle-timer nil)

(defun spell-fu--idle-remove-overlays (&optional point-start point-end)
  "Remove `spell-fu-pending' overlays from current buffer.
If optional arguments POINT-START and POINT-END exist remove overlays from
range POINT-START to POINT-END. Otherwise remove all overlays."
  (remove-overlays point-start point-end 'spell-fu-pending t))

(defun spell-fu--idle-handle-pending-ranges ()
  "Spell check the on-screen overlay ranges."
  (when (bound-and-true-p spell-fu-mode)
    (let*
      ( ;; Don't show the cursor motion from spell checking.
        (visible-start (window-start))
        (visible-end (window-end))
        (overlays-in-view (overlays-in visible-start visible-end)))

      (while overlays-in-view
        (let ((item-ov (pop overlays-in-view)))
          (when
            (and
              (overlay-get item-ov 'spell-fu-pending)
              ;; It's possible these become invalid while looping over items.
              (overlay-buffer item-ov))

            (let
              ( ;; Window clamped range.
                (point-start (max visible-start (overlay-start item-ov)))
                (point-end (min visible-end (overlay-end item-ov))))

              ;; Expand so we don't spell check half a word.
              (spell-fu--setq-expand-range-to-line-boundaries
                ;; Warning these values are set in place.
                point-start point-end)

              (when
                (condition-case err
                  ;; Needed so the idle timer won't quit mid-spelling.
                  (let ((inhibit-quit nil))
                    (funcall spell-fu-check-range point-start point-end)
                    t)
                  (error
                    (progn
                      ;; Keep since this should be very rare.
                      (message "Early exit 'spell-fu-mode': %s" (error-message-string err))
                      ;; Break out of the loop.
                      (setq overlays-in-view nil)
                      nil)))

                ;; Don't delete the overlay since it may extend outside the window bounds,
                ;; always delete the range instead.
                ;;
                ;; While we could remove everything in the window range,
                ;; avoid this because it's possible `spell-fu-check-range' is interrupted.
                ;; Allowing interrupting is important, so users may set this to a slower function
                ;; which doesn't lock up Emacs as this is run from an idle timer.
                (spell-fu--idle-remove-overlays point-start point-end)))))))))

(defun spell-fu--idle-font-lock-region-pending (point-start point-end)
  "Track the range to spell check, adding POINT-START & POINT-END to the queue."
  (let ((item-ov (make-overlay point-start point-end)))
    ;; Handy for debugging pending regions to be checked.
    ;; (overlay-put item-ov 'face '(:background "#000000" :extend t))
    (overlay-put item-ov 'spell-fu-pending t)
    (overlay-put item-ov 'evaporate 't)))

(defun spell-fu--idle-timer-enable ()
  "Add the global idle timer."
  (unless spell-fu--idle-timer
    (setq spell-fu--idle-timer
      (run-with-idle-timer spell-fu-idle-delay t #'spell-fu--idle-handle-pending-ranges))))

(defun spell-fu--idle-timer-disable ()
  "Remove the global idle timer."
  (when spell-fu--idle-timer
    (cancel-timer spell-fu--idle-timer)
    (setq spell-fu--idle-timer nil)))

(defun spell-fu--idle-enable ()
  "Enable the idle style of updating."
  ;; Unlike with immediate style, idle / deferred checking isn't as likely to
  ;; run before fonts have been checked.
  ;; Nevertheless, this avoids the possibility of spell checking
  ;; running before font-faces have been set.
  (spell-fu--with-add-hook-depth-override 100
    (jit-lock-register #'spell-fu--idle-font-lock-region-pending))
  (spell-fu--idle-timer-enable))

(defun spell-fu--idle-disable ()
  "Disable the idle style of updating."
  (jit-lock-unregister #'spell-fu--idle-font-lock-region-pending)
  (spell-fu--remove-overlays)
  (spell-fu--idle-timer-disable)
  (spell-fu--idle-remove-overlays))


;; ---------------------------------------------------------------------------
;; Define Minor Mode
;;
;; Developer note, use global hooks since these run before buffers are loaded.
;; Each function checks if the local mode is active before operating.

(defun spell-fu-mode-enable ()
  "Turn on option `spell-fu-mode' for the current buffer."
  (spell-fu--init-dictionary (spell-fu--dictionary))

  ;; We may want defaults for other modes,
  ;; although keep this general.
  (cond
    ((derived-mode-p 'prog-mode)
      (unless spell-fu-faces-include
        (setq spell-fu-faces-include
          '(font-lock-comment-face font-lock-doc-face font-lock-string-face)))
      (unless spell-fu-faces-exclude
        (setq spell-fu-faces-exclude '(font-lock-constant-face)))))

  (cond
    ((<= spell-fu-idle-delay 0.0)
      (spell-fu--immediate-enable))
    (t
      (spell-fu--idle-enable))))

(defun spell-fu-mode-disable ()
  "Turn off option `spell-fu-mode' for the current buffer."
  (cond
    ((<= spell-fu-idle-delay 0.0)
      (spell-fu--immediate-disable))
    (t
      (spell-fu--idle-disable))))

;;;###autoload
(define-minor-mode spell-fu-mode
  "Toggle `spell-fu-mode' in the current buffer."
  :global nil

  (cond
    (spell-fu-mode
      (spell-fu-mode-enable))
    (t
      (spell-fu-mode-disable))))

(defun spell-fu-mode-turn-on ()
  "Enable the option `spell-fu-mode' where possible."
  (when (and (not (minibufferp)) (not spell-fu-mode))
    (spell-fu-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-spell-fu-mode spell-fu-mode spell-fu-mode-turn-on)

(provide 'spell-fu)
;;; spell-fu.el ends here
