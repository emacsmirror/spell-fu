########
Spell Fu
########

Fast highlighting of misspelled words.

This is a light weight spell checker for Emacs,
that runs from the syntax highlighter without starting external processes.

Available on `Melpa <https://melpa.org/#/spell-fu>`__.


Motivation
==========

There are two main areas this package focuses on.

- Fast on-screen highlighting.

  *Currently most alternative solutions run an external processes and don't always update on-screen results.*

- Hack-able spell-checking.

  *Easily to customize for users, for different major-modes.*


Usage
=====

You may enable this globally which has the following defaults.

.. code-block:: elisp

   (use-package spell-fu)
   (global-spell-fu-mode)

Or you may wish to configure this per-mode, e.g:

.. code-block:: elisp

   (use-package spell-fu)

   (add-hook 'org-mode-hook
     (lambda ()
       (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
       (spell-fu-mode)))

   (add-hook 'emacs-lisp-mode-hook
     (lambda ()
       (spell-fu-mode)))


Details
-------

- All programming modes only check comments and strings.
- All other major modes check all words.
- Currently this package requires ``aspell`` to generate the word-list.


Customization
-------------


Global Settings
^^^^^^^^^^^^^^^

``spell-fu-directory``
   The directory used for storing the dictionary cached.

``spell-fu-idle-delay`` (0.25 seconds)
   The idle time before marking words as misspelled.

   This can be set to zero, in this case an idle timer won't be used,
   and spelling will be checked as part of syntax highlighting.

``spell-fu-incorrect-face`` (red, underline)
   The font to use for the spell checking overlay.


Buffer Local Settings
^^^^^^^^^^^^^^^^^^^^^

You may wish to set these values differently based on the current major-mode.

``spell-fu-syntax-table``
   The syntax table used for spell-checking.

   Useful when the current syntax-table for a major-mode is set for a programming language
   which doesn't make sense to use for natural language.

``spell-fu-word-regexp``
   The regular expression to use for scanning words.

``spell-fu-faces-include``
   When not ``nil``, only faces in this list will be checked.

``spell-fu-faces-exclude``
   When not ``nil``, text with faces in this list will be excluded.


Advanced Buffer Local Settings
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These options allow for applying your own rules to how the spell checker runs,
they aren't necessary for typical usage.

``spell-fu-check-range``
   This is the main function which checks words,
   in most cases this can be left at it's default.

   In some cases you may wish to scan the text in the given range using more sophisticated checks,
   skipping text based on your own rules.

   This function takes ``(point-start point-end)`` arguments,
   which are guaranteed to be on line boundaries.

   ``(spell-fu-check-word point-start point-end word-string)`` should be called for each word you wish to check.

   Note that setting this function causes the following settings to be ignored:

   - ``spell-fu-faces-include``
   - ``spell-fu-faces-exclude``
   - ``spell-fu-word-regexp``
   - ``spell-fu-syntax-table``


Other Settings
--------------

In most cases there is no need to change these settings,
however they will be used when set:

``ispell-program-name``
   Used to call aspell (when this points to ``aspell``, otherwise ``aspell`` is called).

``ispell-dictionary``
   When generating the word-list, this variable is used when present,
   otherwise aspell's default dictionary is used.

``ispell-local-dictionary``
   Setting this allows you to have different languages set per-buffer.

``ispell-personal-dictionary``
   When generating the word-list, this file is included when present.


Commands
--------

While this package is intended to be used with minimal interaction,
there are some commands provided which may come in handy.

``spell-fu-goto-next-error``
   Moves the point to the next error.

``spell-fu-goto-previous-error``
   Moves the point to the previous error.

``spell-fu-buffer``
   Checks spelling for the entire buffer, reporting the number of misspelled words found.

``spell-fu-word-add``
   Add the word under the cursor to the personal dictionary.

``spell-fu-word-remove``
   Remove the word under the cursor from the personal dictionary.


Other Packages
==============

`FlySpell <https://www.emacswiki.org/emacs/FlySpell>`__
   As of Emacs 28, this doesn't provide a way to automatically check all on-screen text,
   and running this on an entire buffer can be slow.

`WCheck Mode <https://github.com/tlikonen/wcheck-mode>`__
   This is a close match to Spell-fu, the main differences is that it's calling a sub-process
   on each word which gives slower results.
   I also found it's configuration rather difficult to manage.

   Spell-fu in contrast takes a different approach,
   instead of exposing many advanced options,
   you can set your own function to extract works from a region of text.


TODO
====

- Support alternates to ``aspell`` for generating word lists.
- Support a custom command for generating a word list.
- Support refreshing the word list at run-time when ispell updates the personal dictionary
  *(currently updates require re-enabling the mode).*
