# emacs-xrandr

Use emacs to configure xrandr.

## install

```
(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))
```

## usage

Simply call `xrandr` interactively, you would be prompted to choose from a list of supported devices and corresponding resolutions. To turn off the previously enabled screen, simply choose the primary device.


The external screen's default postion is the same as the primary screen (mirror). To change this, run `xrandr-set-position` interactively and rerun `xrandr`.
