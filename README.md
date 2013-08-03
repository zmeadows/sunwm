# sunwm

sunwm is a manual tiling window manager for X11/Linux.

## FAQ

#### Q: Why create another tiling window manager?
While there are perhaps an overabundance of dynamic (automatic) tiling window managers, there are far fewer manual tiling window managers. The ones that do exist lack a few features I desired, such as support for floating windows, while simultaneously supporting a great many features that I personally don't find very useful.

#### Q: I was using sunwm and X happened to my Y while I was Z'ing! What's wrong with this piece of junk?
Please consider filing a [bug report](https://github.com/zmeadows/sunwm/issues), no matter how insignificant the issue may seem.

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

<b>focusTo</b> <i>direction</i> <br />
>Switches focus towards <i>direction</i> within the visible tiled windows.
ex: focusTo R

<b>splitV</b> <i>ratio</i> <br />
>Splits the focused frame vertically, yielding two new frames where the new left/right frame sizes are dictated by <i>ratio</i>.

<b>splitH</b> <i>ratio</i> <br />
>Splits the focused frame horizontally, yielding two new frames where the new up/down frame sizes are dictated by <i>ratio</i>.

<b>swapToDir</b> <i>direction</i> <br />
>Swaps the contents of two adjacent frames.

<b>raiseHidden</b> <i>L/R</i> <br />
>Cycles through the stack of hidden windows within the focused frame.

<b>removeFrame</b> <br />
>Removes the focused frame. If a window was contained within the frame, it is added to the hidden stack.

<b>spawnTerminal</b> <br />
>Launches a user-defined terminal.

<b>makeOnly</b> <br />
>Remove all splits from the current workspace, leaving only the focused frame to occupy the entire screen.

<b>killWindow</b> <br />
>Exits the currently focused window. If there are windows in the hidden stack, the next in the queue will be focused. Otherwise, the frame becomes empty.

<b>removeFrame</b> <br />
>Removes the currently focused frame, granting the space it occupied back to the frame(s) it was split with.

<b>equalize</b> <br />
>Sets the split ratios of all frames to 0.5.

<b>banish</b> <br />
>Relocates the mouse pointer to the bottom right of the currently focused screen.

<b>flipT</b> <br />
>Rotates the workspace frame layout by 90 degrees.

<b>shiftTo</b> <i>direction</i> <br />
>Shifts the focused frame towards <i>direction</i>.

<b>resizeFrame</b> <i>direction</i> <i>ratio</i> <br />
>Resizes the frame layout by increase/decreasing the split ratio by <i>ratio</i> in the specified <i>direction</i>.

<b>toggleWS</b> [alt + tab]<br />
>Switches back to the last used workspace.

<b>toggleScr</b> [win + tab] <br />
>Switches focus back to the last used screen.

<b>quit</b> <br />
>Exit sunwm.

<b>changeWS</b> <i>number</i> <br />
>Switches to the workspace specified by <i>number</i>.

<b>moveWinToWS</b> <i>number</i> <br />
>Moves the focused window to the workspace specified by <i>number</i>. The window will be placed into the last frame that was focused on the new workspace.

## acknowledgements

#### X11 window managers

* Ratpoison (http://ratpoison.nongnu.org/)
* StumpWM (http://www.nongnu.org/stumpwm/)
* XMonad (http://xmonad.org/)
* i3 (http://i3wm.org/)
* dwm (http://dwm.suckless.org/)
