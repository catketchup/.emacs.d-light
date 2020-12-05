;; insert images from the plots folder
(defun html-insert-image-from-folder (infolder)
  (interactive "DSelect a folder to import images:\n")
  (let* ((plots (cddr (directory-files infolder))))
    (mapcar (lambda (x)
              (progn (insert (concat
                              "<img class=\"col m6\" src=\"plots/"
                              x
                              "\">\n"))
                     (indent-according-to-mode)))
            plots)))

(provide 'init-html)
