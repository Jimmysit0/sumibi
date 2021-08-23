(use-package buffer-move
  :config
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move)
  :bind
  (("<C-up>" . buf-move-up)
   ("<C-down>" . buf-move-down)
   ("<C-left>" . buf-move-left)
   ("<C-right>" . buf-move-right)))

(use-package windmove
  :bind
  (("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)
   ("M-<left>" .  windmove-left)
   ("M-<right>" . windmove-right)))

(provide '+buffer-move)
