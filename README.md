# xcode-mode

**xcode-mode** is a minor mode for Emacs to perform Xcode like actions in your iOS projects.

## Installation

Requires [Cask](https://github.com/cask/cask)

```
git clone https://github.com/nicklanasa/xcode-mode.git
```

Then, run `cask` to install dependencies:

```
cd xcode-mode
cask
```

## Keybindings

All keybindings start with `C-c C-x`. All functions in xcode-mode have a two-letter mnemonic shortcut, for instance, build-workspace is `bw`.

Here are all the keybindings:

* `C-c C-x bw` builds the current project workspace.
* `C-c C-x bp` builds the current project project.
* `C-c C-x cw` cleans the current project workspace.
* `C-c C-x tw` tests the current project workspace.
* `C-c C-x pi` runs `pod install`.
* `C-c C-x os` displays a list of Storyboard's to open.
* `C-c C-x aw` archives a workspace using the Release configuration.
