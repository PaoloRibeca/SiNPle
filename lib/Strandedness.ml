(*
    Strandedness.ml -- (c) 2017-2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    Which strands the reads are counted on: a directional protocol is evidence
    about one strand, and the other's reads are not evidence about it.

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

include (
  struct
    type t =
      | Forward
      | Reverse
      | Both
    let of_string = function
      | "forward" -> Forward
      | "reverse" -> Reverse
      | "both" -> Both
      | w -> Exception.raise_unrecognized_initializer __FUNCTION__ "strandedness" w
    let to_string = function
      | Forward -> "forward"
      | Reverse -> "reverse"
      | Both -> "both"
    (* What the reader is told to keep: one strand, or nothing said, for both *)
    let to_strand = function
      | Forward -> Some Sequences.Types.forward
      | Reverse -> Some Sequences.Types.reverse
      | Both -> None
  end: sig
    type t =
      | Forward
      | Reverse
      | Both
    val of_string: string -> t
    val to_string: t -> string
    val to_strand: t -> Sequences.Types.strand_t option
  end
)

