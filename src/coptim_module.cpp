

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

