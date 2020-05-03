(* Draft for cherry-picking into pgx.
 *
 * Copyright (C) 2020 Petter A. Urkedal
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version,
 * with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this library; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *)

module Make : functor
  (RANDOM : Mirage_random.S)
  (CLOCK : Mirage_clock.MCLOCK)
  (STACK : Mirage_stack.V4)
  -> sig
  val connect : STACK.t -> (module Pgx_lwt.S.Pgx_impl)
end
