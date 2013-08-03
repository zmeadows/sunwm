# sunwm

sunwm is a manual tiling window manager for X11/Linux.

## FAQ

### Q: Why create another tiling window manager?
While there are perhaps an overabundance of dynamic (automatic) tiling window managers, there are very few manual tiling window managers.

## install

Make sure you have the following installed:

* GHC (http://www.haskell.org/ghc/)
* cabal-install (http://hackage.haskell.org/trac/hackage/wiki/CabalInstall)
* X11 development libraries (http://www.x.org/wiki/)

If it is your first experience with cabal-install, run <code>cabal update</code> before continuing. Then

<pre><code>$ git clone https://github.com/zmeadows/sunwm.git
$ cd sunwm
$ cabal install</pre></code>

Be aware that cabal will install the binary 'sunwm' to $HOME/.cabal/bin, so consider appending this directory to your $PATH.

## configuration

sunwm is configured entirely through the file <code>Sunwm.hs</code>. See the default file for further details.

## keybindings

## acknowledgements/recommendations

#### X11 Window Managers:

* Ratpoison (http://ratpoison.nongnu.org/)
* StumpWM (http://www.nongnu.org/stumpwm/)
* XMonad (http://xmonad.org/)
* i3 (http://i3wm.org/)
* dwm (http://dwm.suckless.org/)
