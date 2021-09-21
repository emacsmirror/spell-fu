
##########
Change Log
##########

- In development.
  - Idle timers now update buffers that have lost focus.
  - Fix for disabling ``spell-fu-mode`` preventing other buffers that have ``spell-fu-mode`` enabled from updating.
  - Changes to the face are now treated as word separators.

    This fixes spell checking in situations where characters are escaped for example ``"test\nterm"``,
    will use the ``\n`` as a divider when escape characters use a different face.
  - ``global-spell-fu-mode`` no longer enables spell-fu for modes derived from ``special-mode``
    such as package list for example (fixes ``#15``).
  - Support conditionally disabling ``global-spell-fu-mode`` via
    ``global-spell-fu-ignore-buffer`` & ``global-spell-fu-ignore-modes``.

- Version 0.3 (2021-03-28)

  - Support affix expansion when calling ``aspell`` (some non English dictionaries use this).
  - Use explicit ``utf-8`` encoding, (fixes issue #8).
  - Improve default ``spell-fu-word-regexp`` to better differentiate quoted text from apostrophes (fixes issue #3).

- Version 0.2 (2020-04-26)

  - Add support personal dictionary manipulation: ``spell-fu-word-add``, ``spell-fu-word-remove``.
  - Add support for stepping over errors with: ``spell-fu-goto-next-error``, ``spell-fu-goto-previous-error``.
  - Add support for checking the whole buffer with: ``spell-fu-buffer``.

- Version 0.1 (2020-04-14)

  Initial release.
