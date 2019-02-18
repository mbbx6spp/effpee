import (builtins.fetchGit (import ./version.nix)) {
  config.allowUnfree = true;
  #overlays = import ../overlays;
}
