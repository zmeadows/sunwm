# SUNWM

sunwm is a manual tiling window manager for X11/Linux.

## FAQ

### Q: Why create another tiling window manager?
While there are perhaps an overabundance of dynamic (automatic) tiling window managers, there are very few manual tiling window managers.


### Q: Something strange happened! What's wrong with this piece of junk?
Please consider filing a [bug report](https://github.com/zmeadows/sunwm/issues), no matter how insignificant the issue may seem.


## INSTALL

Make sure you have the following installed:

* GHC (http://www.haskell.org/ghc/)
* cabal-install (http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
* X11 development libraries (http://www.x.org/wiki/)

The above can all be easily installed through the package manager of any common Linux distribution. If it is your first experience with cabal-install, run <code>cabal update</code> before continuing. Then

<pre><code>$ git clone https://github.com/zmeadows/sunwm.git
$ cd sunwm
$ cabal install</pre></code>

Be aware that cabal will install the binary <code>sunwm</code> to <code>$HOME/.cabal/bin</code>, so consider appending this directory to your $PATH.

## CONFIGURATION

sunwm is configured entirely through the <code>Sunwm.hs</code> file. See the included default file for further details.

## COMMANDS

* focusTo <i>dir</i> - Switches focus towards <i>dir</i> within the visible tiled windows.

## ACKNOWLEDGEMENTS

#### X11 Window Managers:

* Ratpoison (http://ratpoison.nongnu.org/)
* StumpWM (http://www.nongnu.org/stumpwm/)
* XMonad (http://xmonad.org/)
* i3 (http://i3wm.org/)
* dwm (http://dwm.suckless.org/)
