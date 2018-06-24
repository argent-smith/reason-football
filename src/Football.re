/** Score representation for matching operations */
module Score = {
  type t = {
    team_a: int,
    team_b: int
  };

  let of_pair = ((team_a, team_b)) => {team_a, team_b}
};

/** Score matchers */
module Matching = {

  /** Simple pair equality testing */
  let exact_match = (==);

  /** Complex "user guessed the bvalance" matcher */
  let balance_match = (bet, real) => {
    let balance_matcher = (bet, real, (<//>)) => {
      let (bet, real) = Score.(of_pair(bet), of_pair(real));
      bet.team_a <//> bet.team_b && real.team_a <//> real.team_b
    };
    List.(
      [(>), (<), (==)]
      |> map(balance_matcher(bet, real))
      |> fold_left((||), false)
    )
  }
};

/** Discerning whether the guess is valid: the toplevel logic */
module Discernment = {
  type t =
    | Won
    | Guessed
    | Lost;

    /** Naive all-in-one Pervasives-only algorythm */
    let naive_discern = ((team_a_bet, team_b_bet), (team_a_real, team_b_real)) => {
      if (team_a_bet == team_a_real && team_b_bet == team_b_real) {
        Won
      } else {
        if (
          (team_a_bet > team_b_bet && team_a_real > team_b_real)
          || (team_a_bet < team_b_bet && team_a_real < team_b_real)
          || (team_a_bet == team_b_bet && team_a_real == team_b_real)
        ) {
          Guessed
        } else {
          Lost
        }
      }
    };

    /** Modularized pattern-matching idiomatic Stdlib-using (List under the hood) algorythm */
    let discern = (bet, real) => {
      Matching.(
        switch (exact_match(bet, real)) {
        | true => Won
        | false => switch (balance_match(bet, real)) {
                   | true => Guessed
                   | false => Lost
                   }
        }
      )
    };
};

let bookmake = (bet_score, real_score) => {
  Discernment.(
    switch (discern(bet_score, real_score)) {
    | Won => 2
    | Guessed => 1
    | Lost => 0
    } 
  )
};