// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
//
// rcpp_module.cpp: Rcpp R/C++ interface class library -- Rcpp Module examples
//
// Copyright (C) 2010 - 2012  Dirk Eddelbuettel and Romain Francois
//
// This file is part of Rcpp.
//
// Rcpp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// Rcpp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
#include <stdio.h>
#include "Optimization.h"



RCPP_MODULE(coptim) {
    using namespace Rcpp ;
    
    class_<COptimization_>("COptimization_")
        .constructor()
    
        .field("m_VertexCount", &COptimization_::m_VertexCount)
    
        .method("Optimize", &COptimization_::Optimize)
        .method("ParseFileWrap", &COptimization_::ParseFileWrap)
        .method("CategorizeConstraints", &COptimization_::CategorizeConstraints)
        .method("GaussianElimination", &COptimization_::GaussianElimination)
        .method("EnumerateVertices", &COptimization_::EnumerateVertices)
        .method("OutputOptimum", &COptimization_::OutputOptimum)
        .method("Display", &COptimization_::Display)
        
    ;
    
}

