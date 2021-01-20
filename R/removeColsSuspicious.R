## Copyright (C) 2016  Clayton Vieira Fraga Filho
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 2
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
##' @title Create function with generic model
##' @description This function creates a generic model that will be a funcao that has parameters for the variables that can be mapped to each different base. her return will be a generic model that should be mapped to be used by the function avaliaEstimativas
##' @param base is the data with fields to remove
##' @param colsRm is the collections of name fields suspicius to remove of base
##' @return will be returned function with generic model to map to a base
##' @export
removeColsSuspicious <- function(base, colsRm){
  base = base[, setdiff(names(base), colsRm)]
  return (base)
}
