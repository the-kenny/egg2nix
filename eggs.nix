{ pkgs, stdenv }:
rec {
  inherit (pkgs) eggDerivation fetchegg;

  args = eggDerivation {
    name = "args-1.6.0";

    src = fetchegg {
      name = "args";
      version = "1.6.0";
      sha256 = "1dvzdr7sdpfbc8nnzd1hc3wy7pw9plnbbz5b0skz4p96phqxm803";
    };

    buildInputs = [
      srfi-1
      srfi-13
      srfi-37
    ];
  };

  matchable = eggDerivation {
    name = "matchable-1.0";

    src = fetchegg {
      name = "matchable";
      version = "1.0";
      sha256 = "0k2bd5qkcyb3l3gw87g6fx7wyqzb2yvwnlqr5a87nxn9mvqjbg8f";
    };

    buildInputs = [
      
    ];
  };

  srfi-1 = eggDerivation {
    name = "srfi-1-0.5";

    src = fetchegg {
      name = "srfi-1";
      version = "0.5";
      sha256 = "1x78kld3vn9fbnjhk63l7la48fafa6z2nya82mw0k5wnrlfj8c31";
    };

    buildInputs = [
      
    ];
  };

  srfi-13 = eggDerivation {
    name = "srfi-13-0.2";

    src = fetchegg {
      name = "srfi-13";
      version = "0.2";
      sha256 = "03hpgx2drwqxk1n3113q6vpj9d8h08df9sq8n3yhfpw37h6zqvxx";
    };

    buildInputs = [
      srfi-14
    ];
  };

  srfi-14 = eggDerivation {
    name = "srfi-14-0.2";

    src = fetchegg {
      name = "srfi-14";
      version = "0.2";
      sha256 = "0lddw35p1z9fsk6nhn93lj1ljmwx0fjw1yr8g7djw3pk489zjv5b";
    };

    buildInputs = [
      
    ];
  };

  srfi-37 = eggDerivation {
    name = "srfi-37-1.4";

    src = fetchegg {
      name = "srfi-37";
      version = "1.4";
      sha256 = "1liiw2gds3f6b8bwl2qgai9aavz5yl9kvkfs1sh2f27a6r07qzia";
    };

    buildInputs = [
      
    ];
  };
}

