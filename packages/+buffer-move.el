(use-package buffer-move
  :config
  (setq buffer-move-stay-after-swap t)
  (setq buffer-move-behavior 'move)
  :bind
  (("<S-up>" . buf-move-up)
   ("<S-down>" . buf-move-down)
   ("<S-left>" . buf-move-left)
   ("<S-right>" . buf-move-right)))

(use-package windmove
  :bind
  (("M-<up>" . windmove-up)
   ("M-<down>" . windmove-down)
   ("M-<left>" .  windmove-left)
   ("M-<right>" . windmove-right)))

(provide '+buffer-move)
