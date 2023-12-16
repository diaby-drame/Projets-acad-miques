#include "polynome.hpp"
#include <cassert>
#include <math.h>
#include <iostream>

// coder les "grosses" (plusieurs lignes) methodes de la classe ici

Polynome::Polynome(int n){
	
	assert(n>=0);
	degre_ = n;
	coeff = new double [n+1];
}

Polynome::Polynome(const Polynome & P): degre_(P.degre_), coeff(new double [P.degre_+1]){
	
	for(int i = 0; i<degre_+1; i++){
		
		coeff[i] = P.coeff[i];
	}
} 

Polynome::~Polynome(){
	
	delete [] coeff;
}

Polynome & Polynome::operator=(const Polynome & P){
	
	delete this-> coeff;
	this->degre_ = P.degre_;
	this->coeff = new double [degre_+1];
	
	for(int i = 0; i<degre_+1; i++){
		
		coeff[i] = P.coeff[i];
	}
	
	return *this;
}

int Polynome::degre() const{
	
	return this->degre_;
}

double & Polynome::operator[](int i){
	
	assert(0 <= i && i < this->degre_+1);
	return this->coeff[i];
}

double Polynome :: operator[](int i) const{
	
	assert(0 <= i && i < this->degre_+1);
	return this->coeff[i];
}
	

double Polynome::operator()(double x) const{
	
	double S(0.);
	
	for(int i = 0; i<degre_+1; i++){
		
		S += coeff[i]*std::pow(x,i);
	}
	
	return S;
}

Polynome Polynome::operator-() const{
	
	Polynome P(degre_);
	
	for(int i = 0; i<degre_+1; i++){
		
		P.coeff[i] = -coeff[i];
	}
	
	return P;
}

const Polynome operator+(const Polynome & P, const Polynome & Q){
	
	if(P.degre_ < Q.degre_){
		
		Polynome A(Q.degre_);
		
		for(int i = 0; i<P.degre_+1; i++){
			
			A.coeff[i] = P.coeff[i]+Q.coeff[i];
		}
		
		for(int i = P.degre_+1; i<Q.degre_+1; i++){
			
			A.coeff[i] = Q.coeff[i];
		}
		
		return A;
	}
	
	else{
		
		Polynome A(P.degre_);
		
		for(int i = 0; i<Q.degre_+1; i++){
			
			A.coeff[i] = P.coeff[i]+Q.coeff[i];
		}
		
		for(int i = Q.degre_+1; i<P.degre_+1; i++){
			
			A.coeff[i] = P.coeff[i];
		}
		
		return A;
		
	}
			
}

const Polynome operator-(const Polynome & P, const Polynome & Q){
	
	Polynome Z = -Q;
	
	return P+Z;
}

std::ostream & operator<<(std::ostream & o, const Polynome & P){

		
	if(P.degre_ == 1){
		
		if(P.coeff[1] != 0 && P.coeff[0] > 0){
			
			o << P.coeff[1] << "X + " << P.coeff[0] << std::endl;
		}
		
		else if(P.coeff[1] != 0 && P.coeff[0] < 0){
			
			o << P.coeff[1] << "X - " << -P.coeff[0] << std::endl;
		}
		
		else{
			
			o << P.coeff[0] << std::endl;
		}
		
		return o;
	}				
	
	else{
	
		if(P.coeff[P.degre_] > 1 || P.coeff[P.degre_] < -1){
		
			o << P.coeff[P.degre_] << "X^" << P.degre_;
		}
		
		if(P.coeff[P.degre_] == 1){
			
			o << "X^" << P.degre_;
		}
	
		if(P.coeff[P.degre_] == -1){
		
			o << "-X^" << P.degre_;
		}
	
		for(int i = P.degre_-1; i > 1; i--){
		
			if(P.coeff[i] > 1){
			
				o << " + " << P.coeff[i] << "X^" << i;
			}
		
			if(P.coeff[i] < -1){
			
				o << " - " << -P.coeff[i] << "X^" << i;
			}
		
			if(P.coeff[i] == 1){
			
				o << " + " << "X^" << i;
			}
		
			if(P.coeff[i] == -1){
			
				o << " - " << "X^" << i;
			}
				
		}
	
		if(P.coeff[1] == 1){
		
			o << " + "  << "X";
		}
	
		if(P.coeff[1] == -1){
		
			o << " - "  << "X";
		}
	
		if(P.coeff[1] > 1){
		
			o << " + "  << P.coeff[1] <<  "X";
		}
	
		if(P.coeff[1] < -1){
		
			o << " - " << -P.coeff[1] << "X";
		}
		
		if(P.coeff[0] > 0 && P.coeff[1] != 0){
			
			o << " + " << P.coeff[0] << std::endl;
		}
		
		else if(P.coeff[0] < 0 && P.coeff[1] != 0){
			
			o << " - " << -P.coeff[0] << std::endl;
		}
		
		else{
			
			o << P.coeff[0] << std::endl;
		}
	
		return o;
	}
	
}
	
