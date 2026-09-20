(*
    QualitiesDistribution.ml -- (c) 2017-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    The qualities of the reads that voted for a genotype, as a map from quality
    to how many times it was seen, and the statistics the model takes of them.

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open BiOCamLib
open Better

include IntMap
let get_cardinal qs =
  let res = ref 0 in
  IntMap.iter
    (fun _ times ->
      res := !res + times)
    qs;
  !res
let get_tail ?(fraction = 0.5) qs =
  let card = get_cardinal qs in
  let to_discard = int_of_float (floor (float_of_int card *. (1. -. fraction)))
  and discarded = ref 0 and res = ref IntMap.empty in
  IntMap.iter
    (fun qual times ->
      if !discarded < to_discard then begin
        if !discarded + times >= to_discard then begin
          let not_ok = min times (to_discard - !discarded) in
          discarded := !discarded + not_ok;
          if times > not_ok then
            res := IntMap.singleton qual (times - not_ok)
        end else
          discarded := !discarded + times
      end else
        res := IntMap.add qual times !res)
    qs;
  !res
let get_sum qs =
  let res = ref 0 in
  IntMap.iter
    (fun qual times ->
      res := !res + times * qual)
    qs;
  !res
let get_mean qs =
  let acc = ref 0 and counts = ref 0 in
  IntMap.iter
    (fun qual times ->
      counts := !counts + times;
      acc := !acc + times * qual)
    qs;
  float_of_int !acc /. float_of_int !counts
let get_variance qs =
  let card = get_cardinal qs in
  let mean = float_of_int (get_sum qs) /. float_of_int card in
  let res = ref 0 in
  IntMap.iter
    (fun qual times ->
      res := !res + times * qual * qual)
    qs;
  let f_red_card = float_of_int (card - 1) in
  float_of_int !res /. f_red_card -. mean *. mean *. (f_red_card +. 1.) /. f_red_card
let merge qs_1 qs_2 =
  let res = ref qs_1 in
  IntMap.iter
    (fun qual times ->
      try
        res := IntMap.add qual (IntMap.find qual !res + times) !res
      with Not_found ->
        res := IntMap.add qual times !res)
    qs_2;
  !res

