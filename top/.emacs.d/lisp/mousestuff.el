(defun drag-test (ev)
  "test"
  (interactive "@e")
  (track-mouse
    (while (or (mouse-movement-p ev)
               (member 'down (event-modifiers ev)))
      (print ev)
      ;; Read next event
      (setq ev (read-event)))))

(global-unset-key [down-mouse-3])
(global-set-key [down-mouse-3] 'drag-test)
