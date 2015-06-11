Intro
==========================
My own notes on how to setup a GHC environment on windows with MSYS2.
I was put on the right track by following "[Building GHC On Windows](https://ghc.haskell.org/trac/ghc/wiki/Building/Preparation/Windows?redirectedfrom=Building/Preparation/Windows/MSYS2)".

Installing MSYS2 
================================
* Grab the installer from [http://msys2.github.io/]
* Grab either the 32 or 64 bit version. I would suggest the 32 bit version since I still ran into some troubles with 
  64 bit GHC 7.8.4
* Don't install it to a path with spaces in it.
* Follow the steps on the installer's site in particular
    * Update the core MSYS first by running ```pacman -Syu``` from the MSYS shell and then restart the shell.
* Some things to note about the installation
    * In the install directory there are three ```.bat``` files to run 3 different shells (MSYS, MINGW32, MINGW64).
      Most of the time you will run one of the MINGW shells (mingw32_shell.bat if using MSYS2 32 bit)
    * Your home directory would be ```INSTALLATION_DIRECTORY\home\USER_NAME```
    * From within the shell you have a posix like file system in particular 
        * ```C:``` drive is ```/c``` (drives don't show up in root but can all be accessed similarly)
        * Home directory is ```~```
        * You can get the windows path from a shell path by doing ```pwd -W```
* Next install some tools needed for development
    * From the MINGW shell ```pacman -Sy git tar binutils autoconf make libtool automake python2 p7zip patch man wget```
    * Note do not install a compiler tool chain (i.e. gcc) we will use the one that comes with GHC
* Optionally setup public cryptography for use with github etc (openssh should be installed, if not run ```pacman -Sy openssh```)
    * if you already have a public and a private key copy them to ```~/.ssh``` (windows does not like making directories with periods
      do it from the shell ```cd ~; mkdir .ssh```
    * Otherwise generate RSA keys by doing ```ssh-keygen``` .
        * Private key will be ```~/.ssh/id_rsa```
        * Public key will be ```~/.ssh/id_rsa.pub```
    * Setup ssh agent to load the keys when starting the shell.
        * Edit the following file ```~/.bashrc
        * Add the following line to run the ssh agent ``` eval `ssh-agent -s` ```
        * Add the following line to add your keys ``` ssh-add ```

Install GHC
===========================
* Grab pre-built binaries for Windows from [https://www.haskell.org/ghc/]
* At the current time I would suggest 32 bit GHC 7.8.3 [https://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-i386-unknown-mingw32.tar.xz]
* I suggest installing GHC in such a way so that it is easy to switch between distributions 
* Make a directory where you would extract GHC (e.g. ```c:\ghc_all```)
* Extract the versioned GHC folder to that directory (e.g. after extraction you have ```c:\ghc_all\ghc-7.8.3```
* Create a junction called ```ghc_current``` to point at the versioned GHC
    * From the Windows command line and not from the MSYS/MINGW shell.
    * CD to the where the versioned directory was extracted (e.g. ```cd c:\ghc_all``)
    * Create a junction called ghc_current to the versioned directory (e.g. ```mklink /J ghc_current ghc-7.8.3``` )
    * You should now have a folder ```ghc_current``` that points to the specific GHC.
* Now modify the ```.bashrc``` file so that the MSYS/MINGW shell will know about GHC and use the gcc that came with your GHC version.
    * Edit the file ```~/.bashrc``` to add GHC, GHC and cabal to the path by appending 
        * Add GHC : ```export PATH=/c/ghc_all/ghc_current/bin:$PATH```
        * Add GCC that comes with GHC : ```export PATH=/c/ghc_all/ghc_current/mingw/bin:$PATH```
        * Add cabal default binary install directory: ```export PATH=/c/Users/$USER/AppData/Roaming/cabal/bin:$PATH```
* Get the Windows binaries for Cabal from [https://www.haskell.org/cabal/download.html]
    * Extract the binary the versioned GHC binary directory (e.g. ```c:\ghc_all\ghc-7.8.3\bin```
    * The name of the newer built binaries include the full version of cabal so rename it to cabal for easy access.
    * If you install a newer cabal using cabal it will detect and use that one.
    * Update Cabal, from MINGW shell ```cabal update```
    * Edit your Cabal config file to ensure library profiling is enabled
        * Edit the file ```/c/Users/$USER/AppData/Roaming/cabal/config
        * Make sure the file contains the line ```library-profiling: True``` (your probably just have to uncomment it)


