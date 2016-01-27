# xcode-mode

**xcode-mode** is a minor mode for Emacs to perform Xcode like actions in your iOS projects.

## Installation

Requires
* [Cask](https://github.com/cask/cask)
* [xctool](https://github.com/facebook/xctool)

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
* `C-c C-x tp` tests the current project project.
* `C-c C-x pi` runs `pod install`.
* `C-c C-x os` displays a list of Storyboard's to open.
* `C-c C-x aw` archives a workspace using the Release configuration.
* `C-c C-x ap` archives a project using the Release configuration.
* `C-c C-x ow` opens a workspace file in Xcode.
* `C-c C-x op` opens a project file in Xcode.

## Contribute

Yes, please do. Tests are setup but none are written, would love some help here as well.

You'll find the repo at:

    https://github.com/nicklanasa/xcode-mode.git

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/xcode-mode
    $ cask

Run the tests with:

    $ ./run-tests.sh

## Known Issues

* Currently, to get a list of Schemes I'm splitting the output of `xcodebuild -workspace <<workspace>> -list` by the `\n` character. It's not perfect but it does give a list to pick from. This needs to be refined.
* Complilation buffer never finishes after initial run fails or something. Must kill the buffer or something.

## License

Copyright (C) 2015 Nickolas S Lanasa III

Author: Nickolas S Lanasa III <nick@nytekproductions.com>
Keywords: Xcode, iOS

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

