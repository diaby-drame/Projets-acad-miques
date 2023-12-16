#include <iostream>

#ifndef POLYNOMEHEADERDEF
#define POLYNOMEHEADERDEF

class Polynome{
	
	private:
		
		int degre_;
		double *coeff;
		
	public:
	
		Polynome(int n);
		Polynome(const Polynome & P);
		~Polynome();
		
		Polynome & operator=(const Polynome & P);
		int degre() const;
		double & operator[](int i);
		double operator[](int i) const;
		double operator()(double x) const;
		Polynome operator-() const;
		
		friend const Polynome operator+(const Polynome & P, const Polynome & Q);
		friend const Polynome operator-(const Polynome & P, const Polynome & Q);
		friend std::ostream & operator<<(std::ostream & o, const Polynome & P);
	
};

#endif
