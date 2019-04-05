let twoColumnMedia = "(min-width: 34rem)";

module Investor = {
  let component = ReasonReact.statelessComponent("Investors.Investor");
  let make = (~name, _) => {
    ...component,
    render: _self =>
      <div
        className=Css.(
          style([
            display(`flex),
            alignItems(`center),
            backgroundColor(Style.Colors.greyishAlpha(1.0)),
            height(`rem(1.75)),
            marginRight(`rem(0.75)),
            marginBottom(`rem(0.625)),
          ])
        )>
        <h4
          className=Css.(
            merge([
              Style.H3.Technical.title,
              style(Style.paddingX(`rem(0.1875))),
            ])
          )>
          {ReasonReact.string(name)}
        </h4>
      </div>,
  };
};

let component = ReasonReact.statelessComponent("Investors");
let make = _children => {
  ...component,
  render: _self =>
    <div>
      <h3 className=Style.H3.Technical.boxed>
        {ReasonReact.string("O(1) Investors")}
      </h3>
      <div
        className=Css.(
          style([
            maxWidth(`rem(78.)),
            display(`flex),
            flexWrap(`wrap),
            marginTop(`rem(3.)),
            marginLeft(`auto),
            marginRight(`auto),
            justifyContent(`center),
          ])
        )>
        <Investor name="Metastable" />
        <Investor name="Accomplice" />
        <Investor name="Electric Capital" />
        <Investor name="Polychain Capital" />
        <Investor name="Paradigm" />
        <Investor name="Coinbase Ventures" />
        <Investor name="Dragonfly" />
        <Investor name="Dekrypt Capital" />
        <Investor name="Scifi vc" />
        <Investor name="libertus" />
        <Investor name="general catalyst" />
        <Investor name="multicoin capital" />
        <Investor name="collaborative fund" />
        <Investor name="kilowatt capital" />
        <Investor name="naval ravikant" />
        <Investor name="elad gil" />
        <Investor name="linda xie" />
        <Investor name="fred ehrsam" />
        <Investor name="jack herrick" />
        <Investor name="charlie noyes" />
        <Investor name="ed roman" />
        <Investor name="andrew keys" />
        <Investor name="kindred ventures" />
        <Investor name="curious endeavors" />
        <Investor name="blockchange" />
        <Investor name="ogroup" />
        <Investor name="nima capital" />
        <Investor name="evolve vc" />
      </div>
    </div>,
};
