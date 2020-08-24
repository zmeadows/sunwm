# sunwm

sunwm is a manual tiling window manager for X11/Linux, inspired by StumpWM and Ratpoison.

## install

Make sure you have the following installed:

* GHC (http://www.haskell.org/ghc/)
* stack (https://docs.haskellstack.org/en/stable/README/)
* X11 development libraries (http://www.x.org/wiki/)

The above can all be easily installed through the package manager of any common Linux distribution.

<pre><code>
$ git clone https://github.com/zmeadows/sunwm.git
$ cd sunwm
$ stack setup
$ stack build
$ stack install # optional, will put sunwm.exe in PATH
</pre></code>

## configuration

sunwm is configured entirely through the <code>sunwm/app/Main.hs</code> file.
See the included default file <code>Main_default.hs</code> for further details.

## commands

<b>focusTo</b> <i>direction</i> <code>[Ctl t + (h,j,k,l)]</code> <br />
>Switches focus towards <i>direction</i> within the visible tiled windows.
ex: focusTo R

<b>splitV</b> <i>ratio</i> <code>[Ctl t + v]</code> <br />
>Splits the focused frame vertically, yielding two new frames where the new left/right frame sizes are dictated by <i>ratio</i>.

<b>splitH</b> <i>ratio</i> <code>[Ctl t + h]</code> <br />
>Splits the focused frame horizontally, yielding two new frames where the new up/down frame sizes are dictated by <i>ratio</i>.

<b>swapToDir</b> <i>direction</i> <code>[Ctl t + (H,J,K,L)]</code> <br />
>Swaps the contents of two adjacent frames.

<b>raiseHidden</b> <i>L/R</i> <code>[Ctl t + (i,o)]</code> <br />
>Cycles through the stack of hidden windows within the focused frame.

<b>removeFrame</b> <code>[Ctl t + r]</code> <br />
>Removes the focused frame. If a window was contained within the frame, it is added to the hidden stack.

<b>spawnTerminal</b> <code>[Ctl t + c]</code> <br />
>Launches a user-defined terminal.

<b>makeOnly</b> <code>[Ctl t + e]</code> <br />
>Remove all splits from the current workspace, leaving only the focused frame to occupy the entire screen.

<b>killWindow</b> <code>[Ctl t + q]</code> <br />
>Exits the currently focused window. If there are windows in the hidden stack, the next in the queue will be focused. Otherwise, the frame becomes empty.

<b>equalize</b> <code>[Ctl t + =]</code> <br />
>Sets the split ratios of all frames to 0.5.

<b>banish</b> <code>[Ctl t + b]</code> <br />
>Relocates the mouse pointer to the bottom right of the currently focused screen.

<b>flipT</b> <code>[Ctl t + /]</code> <br />
>Rotates the workspace frame layout by 90 degrees.

<b>shiftTo</b> <i>direction</i> <code>[Ctl t + (alt + (h,j,k,l))]</code> <br />
>Shifts the focused frame towards <i>direction</i>.

<b>resizeFrame</b> <i>direction</i> <i>ratio</i> <code>[alt + (H,J,K,L)]</code> <br />
>Resizes the frame layout by increase/decreasing the split ratio by <i>ratio</i> in the specified <i>direction</i>.

<b>toggleWS</b> <code>[alt + tab]</code><br />
>Switches back to the last used workspace.

<b>toggleScr</b> <code>[win + tab]</code> <br />
>Switches focus back to the last used screen.

<b>quit</b> <code>[alt + Q]</code> <br />
>Exit sunwm.

<b>changeWS</b> <i>number</i> <code>[alt + (1,2,3,..)]</code> <br />
>Switches to the workspace specified by <i>number</i>.

<b>moveWinToWS</b> <i>number</i> <code>[alt + shift + (1,2,3,..)]</code> <br />
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

