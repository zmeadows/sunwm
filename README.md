# sunwm

sunwm is a manual tiling window manager for X11/Linux.

## faq

#### Q: Why create another tiling window manager?
While there are perhaps an overabundance of dynamic (automatic) tiling window managers, there are very few manual tiling window managers.


#### Q: Something strange happened! What's wrong with this piece of junk?
Please consider filing a [bug report](https://github.com/zmeadows/sunwm/issues), no matter how insignificant the issue may seem.


## install

Make sure you have the following installed:

* GHC (http://www.haskell.org/ghc/)
* cabal-install (http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
* X11 development libraries (http://www.x.org/wiki/)

The above can all be easily installed through the package manager of any common Linux distribution. If it is your first experience with cabal-install, run <code>cabal update</code> before continuing. Then

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



## acknowledgements

#### X11 window managers

* Ratpoison (http://ratpoison.nongnu.org/)
* StumpWM (http://www.nongnu.org/stumpwm/)
* XMonad (http://xmonad.org/)
* i3 (http://i3wm.org/)
* dwm (http://dwm.suckless.org/)
