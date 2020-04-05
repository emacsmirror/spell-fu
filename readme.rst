########
Spell Fu
########

Fast highlighting of misspelled words.

This is a light weight spell checker for Emacs,
that runs from the syntax highlighter without starting external processes.


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

- Programming modes spell check comments and strings.
- All other major modes check all words.

.. code-block:: elisp

   ;; This package has not yet been accepted into melpa.
   (use-package spell-fu)
   (global-spell-fu-mode)

Or you may wish to configure this per-mode, e.g:

.. code-block:: elisp

   (use-package spell-fu)

   (add-hook 'org-mode-hook
     (lambda ()
       (setq spell-fu-faces-exclude '(org-meta-line org-link org-code))
       (spell-fu-mode)))


Details
-------

- Currently this package requires ``aspell`` to generate the word-list.


Customization
-------------

``spell-fu-directory``
   The directory used for storing the dictionary cached.

``spell-fu-idle-delay`` (0.25 seconds)
   The idle time before marking words as misspelled.

   This can be set to zero, in this case an idle timer won't be used,
   and spelling will be checked as part of syntax highlighting.

``spell-fu-incorrect-face`` (red, underline)
   The font to use for the spell checking overlay.

``spell-fu-syntax-table`` (buffer-local)
   The syntax table used for spell-checking.

   Useful when the current syntax-table for a major-mode is set for a programming language
   which doesn't make sense to use for natural language.

``spell-fu-word-regexp``
   The regular expression to use for scanning words.

``spell-fu-faces-include``
   When not ``nil``, only faces that in this list will be checked.

``spell-fu-faces-exclude``
   When not ``nil``, text with faces in this list will be excluded.

``spell-fu-check-range``
   This is the main function which checks words,
   in most cases this can be left at it's default.

   In some cases you may wish to scan the text in the given range using more sophisticated checks,
   skipping text based on your own rules.

   This function takes ``(point-start point-end)`` arguments,
   which are guaranteed to be on line boundaries.

   ``(spell-fu-check-word point-start point-end word-string)`` should be called for each word you wish to check.

   Note that setting this function causes the following settings to be ignored:

   - ``spell-fu-word-regexp``
   - ``spell-fu-faces-include``
   - ``spell-fu-faces-exclude``


Other Settings
--------------

Some settings are used which aren't part of this package:

``ispell-program-name``
   Used to call aspell (when this points to ``aspell``, otherwise ``aspell`` is called).

``ispell-dictionary``
   When generating the word-list, this variable is used when present,
   otherwise aspell's default dictionary is used.

``ispell-personal-dictionary``
   When generating the word-list, this file is included when present.


Limitations
===========

TODO.


Installation
============

TODO.


Other Packages
==============

TODO.


TODO
====

- Support alternates to ``aspell`` for generating word lists.
- Support a custom command for generating a word list.
- Support buffer local dictionaries.
