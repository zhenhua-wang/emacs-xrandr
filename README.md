# emacs-xrandr

Use emacs to configure xrandr.

## install

```
(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))
```

## usage

Simply call `xrandr` interactively, you would be prompted to choose from a list of supported devices and corresponding resolutions.


The external screen's default postion is the same as the primary screen (mirror). Use `xrandr-set-position` to change it.
