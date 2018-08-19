module Tick_curve = Snarky.Backends.Mnt4.GM
module Tock_curve = Snarky.Backends.Mnt6.GM

module Inner_curve = Snarky.Libsnark.Mnt6.Group

(*
module Hash_curve = struct
  (*
  Curve params:
  d = 20
  cardinality = 475922286169261325753349249653048451545124878135421791758205297448378458996221426427165320
  2^3 * 5 * 7 * 399699743 * 4252498232415687930110553454452223399041429939925660931491171303058234989338533 *)

  open Tick_curve

  let d = Field.of_int 20

  let cofactor =
    Bignum_bigint.(of_int 8 * of_int 5 * of_int 7 * of_int 399699743)

  let order =
    Bignum_bigint.of_string
      "4252498232415687930110553454452223399041429939925660931491171303058234989338533"

  let generator =
    let f s = Bigint.R.(to_field (of_decimal_string s)) in
    ( f
        "327139552581206216694048482879340715614392408122535065054918285794885302348678908604813232"
    , f
        "269570906944652130755537879906638127626718348459103982395416666003851617088183934285066554"
    )
   end *)

module Pedersen_params = Pedersen_params
