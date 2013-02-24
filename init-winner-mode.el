(winner-mode 1)

(fill-keymap winner-mode-map
             "C-x 7" 'winner-undo
             "C-x 9" 'winner-redo)
