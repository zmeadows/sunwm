# sunwm

sunwm is a manual tiling window manager for X11/Linux.

## install

Make sure you have the following installed:

* GHC (http://www.haskell.org/ghc/)
* cabal-install (http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
* X11 development libraries (http://www.x.org/wiki/)

The above can all be easily installed through the package manager of any common Linux distribution. If it is your first experience with cabal-install, run <code>cabal update</code> before continuing in order to download the [Hackage](http://hackage.haskell.org/packages/hackage.html) database. Then

<pre><code>$ git clone https://github.com/zmeadows/sunwm.git
$ cd sunwm
$ cabal install</pre></code>

Be aware that cabal will install the binary <code>sunwm</code> to <code>$HOME/.cabal/bin</code>, so consider appending this directory to your $PATH.

## configuration

sunwm is configured entirely through the <code>Sunwm.hs</code> file. See the included default file for further details.

## commands

<b>focusTo</b> <i>direction</i> [Ctl t + (h,j,k,l)] <br />
>Switches focus towards <i>direction</i> within the visible tiled windows.
ex: focusTo R

<b>splitV</b> <i>ratio</i> [Ctl t + v] <br />
>Splits the focused frame vertically, yielding two new frames where the new left/right frame sizes are dictated by <i>ratio</i>.

<b>splitH</b> <i>ratio</i> [Ctl t + h] <br />
>Splits the focused frame horizontally, yielding two new frames where the new up/down frame sizes are dictated by <i>ratio</i>.

<b>swapToDir</b> <i>direction</i> [Ctl t + (H,J,K,L)] <br />
>Swaps the contents of two adjacent frames.

<b>raiseHidden</b> <i>L/R</i> [Ctl t + (i,o)] <br />
>Cycles through the stack of hidden windows within the focused frame.

<b>removeFrame</b> [Ctl t + r] <br />
>Removes the focused frame. If a window was contained within the frame, it is added to the hidden stack.

<b>spawnTerminal</b> [Ctl t + c] <br />
>Launches a user-defined terminal.

<b>makeOnly</b> [Ctl t + e] <br />
>Remove all splits from the current workspace, leaving only the focused frame to occupy the entire screen.

<b>killWindow</b> [Ctl t + q] <br />
>Exits the currently focused window. If there are windows in the hidden stack, the next in the queue will be focused. Otherwise, the frame becomes empty.

<b>equalize</b> [Ctl t + =] <br />
>Sets the split ratios of all frames to 0.5.

<b>banish</b> [Ctl t + b] <br />
>Relocates the mouse pointer to the bottom right of the currently focused screen.

<b>flipT</b> [Ctl t + /] <br />
>Rotates the workspace frame layout by 90 degrees.

<b>shiftTo</b> <i>direction</i> [Ctl t + (alt + (h,j,k,l))] <br />
>Shifts the focused frame towards <i>direction</i>.

<b>resizeFrame</b> <i>direction</i> <i>ratio</i> [alt + (H,J,K,L)] <br />
>Resizes the frame layout by increase/decreasing the split ratio by <i>ratio</i> in the specified <i>direction</i>.

<b>toggleWS</b> [alt + tab]<br />
>Switches back to the last used workspace.

<b>toggleScr</b> [win + tab] <br />
>Switches focus back to the last used screen.

<b>quit</b> [alt + Q] <br />
>Exit sunwm.

<b>changeWS</b> <i>number</i> [alt + (1,2,3,..)] <br />
>Switches to the workspace specified by <i>number</i>.

<b>moveWinToWS</b> <i>number</i> [alt + shift + (1,2,3,..)] <br />
>Moves the focused window to the workspace specified by <i>number</i>. The window will be placed into the last frame that was focused on the new workspace.

## recommendations

#### X11 window managers

* Ratpoison (http://ratpoison.nongnu.org/)
* StumpWM (http://www.nongnu.org/stumpwm/)
* XMonad (http://xmonad.org/)
* i3 (http://i3wm.org/)
* dwm (http://dwm.suckless.org/)

#### Utilities

* xmobar (http://projects.haskell.org/xmobar/)
* dmenu (http://tools.suckless.org/dmenu/)

#### Misc

* Haskell Programming Language (http://www.haskell.org/haskellwiki/Haskell)
