# Tickler


## Installation

### Cloning

Clone the repository

``` shell
git clone https://github.com/NorfairKing/tickler.git --recursive
```

### Building

#### With Nix

To install only the command-line application `tickler`:

``` shell
nix-env --install --file nix/pkgs.nix --attr ticklerPackages.tickler-cli
```

To also install the other tickler applications like the server and web server:

``` shell
nix-env --install --file nix/pkgs.nix --attr ticklerPackages
```


#### With stack

Follow the instructions at https://docs.haskellstack.org/en/stable/README/
to install `stack`.
For example:

``` shell
curl -sSL https://get.haskellstack.org/ | sh
```

Then install `autoexporter`:

``` shell
stack install autoexporter
```

Finally, install the tickler cli:

``` shell
stack install :tickler
```

To also install the other tickler applications like the server and web server:

``` shell
stack install
```

### Troubleshooting 

#### Permission Denied (publickey)

If you see an error like this during cloning:

```
Permission Denied (publickey)
```

You probably used ssh-based cloning instead of https-based cloning.
Make sure to use the clone command as shown.

#### Could not execute autoexporter

If you see an error like this during building with stack:

```
tickler-data > ghc: could not execute: autoexporter
```

You forgot to run `stack install autoexporter`.

#### Aeson exception

If you see an error like this during building with stacK:

```
Aeson exception:
Error in $.packages[10].completed: failed to parse field 'packages': failed to parse field 'completed': [...]
```

You probably cloned an old version and `git pull`-ed recently.
You still need to remove `stack.yaml.lock`.
You will only need to do this once.


## Configuration

### Options

Every configuration option can be specified using command-line flags as well.
See `tickler --help` or `tickler <command> --help` for more information about the options for each command.

```
--cache-dir:      The cache dir
--data-dir:       The data dir
--sync:           Definitely sync every time it's appropriate
--no-sync:        Never try to sync automatically
--url:            The sync server api url
--username:       The sync username
--password:       The sync password
--config-file     Use this custom config file
```

Every option can also be specified via environment variables.

```
TICKLER_CONFIG_FILE:    Config file
TICKLER_CACHE_DIR:      The cache dir
TICKLER_DATA_DIR:       The data dir
TICKLER_SYNC_STRATEGY:  The sync strategy, 'NeverSync' or 'AlwaysSync'
TICKLER_URL:            The sync server api url
TICKLER_USERNAME:       The sync username
TICKLER_PASSWORD:       The sync password
```

Every option can also be specified in the config file.

```
cache-dir:      The cache dir
data-dir:       The data dir
sync:           The sync strategy
url:            The sync server api url
username:       The sync username
password:       The sync password
```


### Config file location

The `tickler` cli application looks for config files in these locations by default, in order:

```
- $XDG_CONFIG_HOME/tickler/config.yaml
- $HOME/.config/tickler/config.yaml
- $HOME/.tickler/config.yaml
```


## Setting up synchronisation with `tickler.cs-syd.eu`

Put this in your config file:

```
url: 'https://api.tickler.cs-syd.eu'
username: 'YOUR USERNAME HERE'
```

Then register:

``` shell
tickler register
```

and login:

``` shell
tickler login
```

Now you can sync manually:

``` shell
tickler sync
```

Note that syncing will occur automatically any time you change anything locally.
If you would prefer to schedule syncing manually to decrease latency locally, you can use a different syncing strategy:

```
sync: NeverSync
```


### Setting up tickler in Nix Home Manager

Within your `home.nix`, add the tickler module from this repository:

``` nix
{ pkgs, lib, ... }:
with lib;
let
  ticklerModule = (builtins.fetchGit {
    url = "https://github.com/NorfairKing/tickler";
    ref = "master";
    rev = "0000000000000000000000000000000000000000"; # Add a recent version here.
  } + "/nix/home-manager-module.nix");
in
{
  imports = [
    ticklerModule
    # [...]
  ];
  programs.tickler = {
    enable = true;
    sync = {
      enable = true;
      username = "YOUR_USERNAME_HERE";
      password = "YOUR_PASSWORD_HERE;
    };
  };
}
```

Note that we have to use `builtins.fetchGit` and cannot use `fetchFromGitHub` because this needs to be fetched at evaluation time.

