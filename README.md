# emacs-xrandr

Use emacs to configure xrandr.

## install

```
(use-package emacs-xrandr
  :straight (:host github :repo "zhenhua-wang/emacs-xrandr"))
```

## usage

Simply call `xrandr` interactively, you would be prompted to choose from a list of supported devices and corresponding resolutions.


`xrandr-position` can be used to set screen position. It has five options:
1. `t` - mirror
2. `'top` - top
3. `'bottom` - bottom
4. `'left` - left
5. `'right` - right
