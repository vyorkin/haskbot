let
  lib = (import ./release.nix).haskell.lib;
in
(super: {
  servant = lib.dontCheck (super.callPackage ./deps/servant.nix {});
  # servant-server = super.callPackage ./deps/servant-server.nix {};
  servant-client = lib.dontCheck (super.callPackage ./deps/servant-client.nix {});
  servant-client-core = lib.dontCheck (super.callPackage ./deps/servant-client-core.nix {});

  telegram-bot-simple = lib.dontCheck (super.callCabal2nix "telegram-bot-simple" ../vendor/telegram-bot-simple {});
})
