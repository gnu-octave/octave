((nil .
      ((c-file-style . "gnu")
       (indent-tabs-mode . nil)
       (fill-column . 72)
       (eval . (when (string-match "\\.h\\'" (buffer-file-name))
                   (c++-mode)
                   (c-set-style "gnu")))))
 (change-log-mode . ((indent-tabs-mode . t)))
 (makefile-mode . ((indent-tabs-mode . t))))
