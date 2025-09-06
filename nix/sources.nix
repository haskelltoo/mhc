{
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-25.05.tar.gz";
    sha256 = "1cllhzs1263vyzr0wzzgca0njvfh04p1mf6xiq2vfd1nbr7jinpa";
  };
}
