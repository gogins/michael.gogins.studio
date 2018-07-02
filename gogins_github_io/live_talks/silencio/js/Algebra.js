/*
* Author : Martin Donk
* Website : http://www.nerdamer.com
* Email : martin.r.donk@gmail.com
* License : MIT
* Source : https://github.com/jiggzson/nerdamer
*/

if((typeof module) !== 'undefined') {
    nerdamer = require('./nerdamer.core.js');
}

(function() {
    "use strict";
    
    /*shortcuts*/
    var core = nerdamer.getCore(),
        _ = core.PARSER,
        N = core.groups.N,
        S = core.groups.S,
        EX = core.groups.EX,
        FN = core.groups.FN,
        PL = core.groups.PL,
        CP = core.groups.CP,
        CB = core.groups.CB,
        keys = core.Utils.keys,
        variables = core.Utils.variables,
        round = core.Utils.round,
        Frac = core.Frac,
        isInt = core.Utils.isInt,
        Symbol = core.Symbol,
        EPSILON = core.Settings.EPSILON;
        
        /**
        * Converts a symbol into an equivalent polynomial arrays of 
        * the form [[coefficient_1, power_1],[coefficient_2, power_2], ... ]
        * @param {Symbol|Number} symbol
        * @param {String} variable The variable name of the polynomial
        * @param {int} order
        */
        function Polynomial(symbol, variable, order) { 
            if(core.Utils.isSymbol(symbol)) {
                this.parse(symbol);
            }
            else if(!isNaN(symbol)) { 
                order = order || 0;
                if(variable === undefined) 
                    throw new Error('Polynomial expects a variable name when creating using order');
                this.coeffs = [];
                this.coeffs[order] = symbol;
                this.fill(symbol);
            }
            else if(typeof symbol === 'string') {
                this.parse(_.parse(symbol));
            }
            
        }
        
        Polynomial.fromArray = function(arr, variable) {
            if(typeof variable === 'undefined') 
                throw new Error('A variable name must be specified when creating polynomial from array');
            var p = new Polynomial();
            p.coeffs = arr;
            p.variable = variable;
            return p;
        };
        
        Polynomial.prototype = { 
            parse: function(symbol, c) { 
                this.variable = variables(symbol)[0]; 
                if(!symbol.isPoly()) throw new Error('Polynomial Expected! Received '+core.Utils.text(symbol));
                c = c || [];
                if(!symbol.power.absEquals(1)) symbol = _.expand(symbol);

                if(symbol.group === core.groups.N) { c[0] = symbol.multiplier; }
                else if(symbol.group === core.groups.S) { c[symbol.power.toDecimal()] = symbol.multiplier; }
                else { 
                    for(var x in symbol.symbols) { 
                        var sub = symbol.symbols[x],
                            p = sub.power; 
                        if(core.Utils.isSymbol(p)) throw new Error('power cannot be a Symbol');
                        
                        p = sub.group === N ? 0 : p.toDecimal();
                        if(sub.symbols){ 
                            this.parse(sub, c);  
                        }
                        else { 
                            c[p] = sub.multiplier; 
                        }
                    }
                }
                
                this.coeffs = c;
                
                this.fill();
            },
            /**
            * Fills in the holes in a polynomial with zeroes
            * @param {Number} x - The number to fill the holes with
            */
            fill: function(x) {
                x = Number(x) || 0;
                var l = this.coeffs.length;
                for(var i=0; i<l; i++) {
                    if(this.coeffs[i] === undefined) { this.coeffs[i] = new Frac(x); }
                }
                return this;
            },
            /**
            * Removes higher order zeros or a specific coefficient
            * @returns {Array}
            */
            trim: function() { 
                var l = this.coeffs.length;
                while(l--) {
                    var c = this.coeffs[l];
                    if(c && c.equals(0)) this.coeffs.pop();
                    else break;
                }

                return this;
            },
            /**
            * Adds together 2 polynomials
            * @param {Polynomial} poly
            */
            add: function(poly) {
                var l = Math.max(this.coeffs.length, poly.coeffs.length);
                for(var i=0; i<l; i++) {
                    var a = (this.coeffs[i] || new Frac(0)),
                        b = (poly.coeffs[i] || new Frac(0));
                    this.coeffs[i] = a.add(b);
                }
                return this;
            },
            /**
            * Adds together 2 polynomials
            * @param {Polynomial} poly
            */
            subtract: function(poly) {
                var l = Math.max(this.coeffs.length, poly.coeffs.length);
                for(var i=0; i<l; i++) {
                    var a = (this.coeffs[i] || new Frac(0)),
                        b = (poly.coeffs[i] || new Frac(0));
                    this.coeffs[i] = a.subtract(b);
                }
                return this;
            },
            divide: function(poly) {
                var variable = this.variable,
                    dividend = core.Utils.arrayClone(this.coeffs),
                    divisor = core.Utils.arrayClone(poly.coeffs),
                    n = dividend.length,
                    mp = divisor.length-1,
                    quotient = [];

                //loop through the dividend
                for(var i=0; i<n; i++) {
                    var p = n-(i+1);
                    //get the difference of the powers
                    var d = p - mp;
                    var inBrackets = core.Utils.inBrackets;
                    //get the quotient of the coefficients
                    var q = dividend[p].divide(divisor[mp]);

                    if(d < 0) break;//the divisor is not greater than the dividend
                    //place it in the quotient
                    quotient[d] = q;

                    for(var j=0; j<=mp; j++) {
                        //reduce the dividend
                        dividend[j+d] = dividend[j+d].subtract((divisor[j].multiply(q)));
                    }
                }

                //clean up
                var p1 = Polynomial.fromArray(dividend, variable || 'x').trim(), //pass in x for safety
                    p2 = Polynomial.fromArray(quotient, variable || 'x');
                return [p2, p1];
            },
            multiply: function(poly) {
                var l1 = this.coeffs.length, l2 = poly.coeffs.length, 
                    c = []; //array to be returned
                for(var i=0; i<l1; i++) {
                    var x1 = this.coeffs[i];
                    for(var j=0; j<l2; j++) {
                        var k = i+j, //add the powers together
                            x2 = poly.coeffs[j],
                            e = c[k] || new Frac(0); //get the existing term from the new array
                        c[k] = e.add(x1.multiply(x2)); //multiply the coefficients and add to new polynomial array
                    }
                }
                this.coeffs = c;
                return this;
            },
            /**
             * Checks if a polynomial is zero
             * @returns {Boolean}
             */
            isZero: function() {
                var l = this.coeffs.length;
                for(var i=0; i<l; i++) {
                    var e = this.coeffs[i];
                    if(!e.equals(0)) return false;
                }
                return true;
            },
            sub: function(n) {
                var sum = new Frac(0), l=this.coeffs.length;
                for(var i=0; i<l; i++) {
                    var t = this.coeffs[i];
                    if(!t.equals(0)) sum = sum.add(t.multiply(new Frac(Math.pow(n, i))));
                }
                return sum;
            },
            clone: function() {
                var p = new Polynomial();
                p.coeffs = this.coeffs;
                p.variable = this.variable;
                return p;
            },
            deg: function() {
                this.trim();
                return this.coeffs.length-1;
            },
            lc: function() { 
                return this.coeffs[this.deg()].clone();
            },
            monic: function() {
                var lc = this.lc(), l = this.coeffs.length; 
                for(var i=0; i<l; i++) this.coeffs[i] = this.coeffs[i].divide(lc);
                return this;
            },
            gcd: function(poly) { 
                //get the maximum power of each
                var mp1 = this.coeffs.length-1, 
                    mp2 = poly.coeffs.length-1,
                    T;
                //swap so we always have the greater power first
                if(mp1 < mp2) {
                    return poly.gcd(this);
                }
                var a = this;

                while(!poly.isZero()) {   
                    var t = poly.clone(); 
                    a = a.clone(); 
                    T = a.divide(t);
                    poly = T[1]; 
                    a = t; 
                }
                
                var gcd = core.Math2.QGCD.apply(null, a.coeffs);

                if(!gcd.equals(1)) { 
                    var l = a.coeffs.length;
                    for(var i=0; i<l; i++) {
                        a.coeffs[i] = a.coeffs[i].divide(gcd);
                    }
                }
                return a;
            },
            polyGCD: function(poly) {
                
            },
            /**
             * Differentiates the polynomial
             */
            diff: function() {
                var new_array = [], l = this.coeffs.length;
                for(var i=1; i<l; i++) new_array.push(this.coeffs[i].multiply(new Frac(i)));
                this.coeffs = new_array;
                return this;
            },
            /**
             * Integrates the polynomial
             */
            integrate: function() {
                var new_array = [0], l = this.coeffs.length;
                for(var i=0; i<l; i++) {
                    var c = new Frac(i+1);
                    new_array[c] = this.coeffs[i].divide(c);
                }
                this.coeffs = new_array;
                return this;
            },
            gcf: function(toPolynomial) {
                //get the first nozero coefficient and returns its power
                var fnz = function(a) {
                        for(var i=0; i<a.length; i++)
                            if(!a[i].equals(0)) return i;
                    },
                    ca = [];
                for(var i=0; i<this.coeffs.length; i++) {
                    var c = this.coeffs[i];
                    if(!c.equals(0) && ca.indexOf(c) === -1) ca.push(c);
                }
                var p = [core.Math2.QGCD.apply(undefined, ca), fnz(this.coeffs)].toDecimal(); 
                
                if(toPolynomial) {
                    var parr = [];
                    parr[p[1]-1] = p[0];
                    p = Polynomial.fromArray(parr, this.variable).fill();
                }
                
                return p;
            },
            /**
             * Raises a polynomial P to a power p -> P^p. e.g. (x+1)^2
             * @param {Int} p - The power to be raised to 
             *
            raiseTo: function(p) {
                var a = this.coeffs.slice(), ll=this.coeffs.length;
                while(--p) { 
                    var l = a.length, r=[];
                    for(var i=0; i<l; i++) {
                        for(var j=0; j<ll; j++) {
                            var idx = i+j, e = r[idx] || 0;
                            r[idx] = e+a[i]*this.coeffs[j];
                        }
                    }
                    a = r;
                }
                this.coeffs = r;
                return this;
            },*/
            quad: function(incl_img) {
                var roots = [];
                if(this.coeffs.length > 3) throw new Error('Cannot calculate quadratic order of '+(this.coeffs.length-1));
                if(this.coeffs.length === 0) throw new Error('Polynomial array has no terms');
                var a = this.coeffs[2] || 0, b = this.coeffs[1] || 0, c = this.coeffs[0];
                var dsc = b*b-4*a*c;
                if(dsc < 0 && !incl_img) return roots;
                else {
                    roots[0] = (-b+Math.sqrt(dsc))/(2*a);
                    roots[1] = (-b-Math.sqrt(dsc))/(2*a);
                }
                return roots;
            },
            /**
             * Converts polynomial to Symbol
             * @returns {Symbol}
             */
            toSymbol: function() {
                var l = this.coeffs.length,
                    variable = this.variable;
                if(l === 0) return new core.Symbol(0);
                var end = l -1, str = '';

                for(var i=0; i<l; i++) {
                    //place the plus sign for all but the last one
                    var plus = i === end ? '' : '+',
                        e = this.coeffs[i];
                    if(!e.equals(0)) str += (e+'*'+variable+'^'+i+plus);
                }
                return _.parse(str);
            },
            equalsNumber: function(x) {
                this.trim();
                return this.coeffs.length === 1 && this.coeffs[0] === x;
            },
            toString: function() {
                return this.toSymbol().toString();
            }
        };
        
    //make the polynomial class available to EVERYONE!
    core.Polynomial = Polynomial;
    
    /**
    * If the symbols is of group PL or CP it will return the multipliers of each symbol
    * as these are polynomial coefficients. CB symbols are glued together by multiplication
    * so the symbol multiplier carries the coefficients for all contained symbols.
    * For S it just returns it's own multiplier. This function doesn't care if it's a polynomial or not
    * @param {Array} c The coefficient array
    * @return {Array}
    */
    Symbol.prototype.coeffs = function(c, with_order) {
        if(with_order && !this.isPoly(true)) _.error('Polynomial expected when requesting coefficients with order');
        c = c || [];
        var s = this.clone().distributeMultiplier(); 
        if(s.isComposite()) {
            for(var x in s.symbols) { 
                var sub = s.symbols[x];
                if(sub.isComposite()) { 
                    sub.clone().distributeMultiplier().coeffs(c, with_order);
                }
                else { 
                    if(with_order) c[sub.isConstant() ? 0 : sub.power.toDecimal()] = sub.multiplier;
                    else c.push(sub.multiplier);
                }
            }
        }
        else { 
            if(with_order) c[s.isConstant() ? 0 : s.power.toDecimal()] = s.multiplier;
            else c.push(s.multiplier);
        }
        //fill the holes
        if(with_order) {
            for(var i=0; i<c.length; i++)
                if(c[i] === undefined) c[i] = new Frac(0);
        }
        
        return c;
    };
    Symbol.LSORT = function(a, b) {
        if(a.value === b.value && a.multiplier > b.multiplier) return b.power - a.power;
        return (a.length || 1) - (b.length || 1);
    };
    Symbol.prototype.altVar = function(x) {
        var m = this.multiplier.toString(), p = this.power.toString();
        return (m === '1' ? '' : m+'*')+ x + (p === '1' ? '' : '^'+p);
    };
    Symbol.prototype.hasFunc = function() {
        if(this.group === FN || this.group === EX) return true;
        if(this.symbols) {
            for(var x in this.symbols) {
                if(this.symbols[x].hasFunc()) return true;
            }
        }
        return false;
    };
    Symbol.prototype.hasConstant = function() {
        if(this.group === CP) {
            for(var x in this.symbols) {
                if(this.symbols[x].isConstant()) return true;
            }
        }
        return false;
    };
    core.Utils.subFunctions = function(symbol, map) {
        map = map || {};
        var subbed = [];
        symbol.each(function(x) {

            if(x.group === FN || x.previousGroup === FN) {
                //we need a new variable name so why not use one of the existing
                var val = core.Utils.text(x, 'hash'), tvar = map[val];
                if(!tvar) {
                    //generate a unique enough name
                    var t = x.fname+core.Utils.keys(map).length;
                    map[val] = t;
                    subbed.push(x.altVar(t));
                }
                else subbed.push(x.altVar(tvar));
            }
            else if(x.group === CB || x.group === PL || x.group === CP) {
                subbed.push(core.Utils.subFunctions(x, map));
            }
            else subbed.push(x.text());
        });
        if(symbol.group === CP || symbol.group === PL) return symbol.altVar(core.Utils.inBrackets(subbed.join('+')));;
        if(symbol.group === CB) return symbol.altVar(core.Utils.inBrackets(subbed.join('*')));
        return symbol.text();
    };
    /**
     * A debugging method to be stripped
     * @returns {String}
     */    
    var qc = function() {
        var args = [].slice.call(arguments),
            name = args.shift();
        args = args.map(function(a) {
            return __.polyArray2Symbol(a, 'x').text();
        });
        
        return name+'('+args.join(',')+')';
    };
    var __ = core.Algebra = {

        version: '1.3.3',
        init: (function() {})(),
        proots: function(symbol, decp) { 
            //the roots will be rounded up to 7 decimal places.
            //if this causes trouble you can explicitly pass in a different number of places
            //rarr for polynomial of power n is of format [n, coeff x^n, coeff x^(n-1), ..., coeff x^0]
            decp = decp || 7;
            var zeros = 0;
            var get_roots = function(rarr, powers, max) {
                var roots = calcroots(rarr, powers, max);
                for(var i=0;i<zeros;i++) roots.unshift(0);
                return roots;
            };
            
            if(symbol instanceof Symbol && symbol.isPoly()) { 
                if(symbol.group === core.groups.S) { 
                    return [0];
                }
                else if(symbol.group === core.groups.PL) { 
                    var powers = keys(symbol.symbols),
                        minpower = core.Utils.arrayMin(powers),
                    symbol = core.PARSER.divide(symbol, core.PARSER.parse(symbol.value+'^'+minpower));
                }
                var variable = keys(symbol.symbols).sort().pop(), 
                    sym = symbol.group === core.groups.PL ? symbol.symbols : symbol.symbols[variable], 
                    g = sym.group,
                    powers = g === S ? [sym.power.toDecimal()] : keys(sym.symbols),
                    rarr = [],
                    max = core.Utils.arrayMax(powers); //maximum power and degree of polynomial to be solved

                // Prepare the data
                for(var i=1; i<=max; i++) { 
                    var c = 0; //if there is no power then the hole must be filled with a zero
                    if(powers.indexOf(i+'') !== -1) { 
                        if(g === S) { 
                            c = sym.multiplier; 
                        }
                        else {
                            c = sym.symbols[i].multiplier;
                        }
                    }
                    // Insert the coeffient but from the front
                    rarr.unshift(c);
                }

                rarr.push(symbol.symbols['#'].multiplier);

                if(sym.group === S) rarr[0] = sym.multiplier;//the symbol maybe of group CP with one variable

                return get_roots(rarr, powers, max);
            }
            else if(core.Utils.isArray(symbol)) {
                var parr = symbol;
                var rarr = [],
                    powers = [],
                    last_power = 0;
                for(var i=0; i<parr.length; i++) {
                    
                    var coeff = parr[i][0],
                        pow = parr[i][1],
                        d = pow - last_power - 1;
                    //insert the zeros
                    for(var j=0; j<d; j++) rarr.unshift(0);
                    
                    rarr.unshift(coeff);
                    if(pow !== 0) powers.push(pow);
                    last_power = pow;
                }
                var max = Math.max.apply(undefined, powers);

                return get_roots(rarr, powers, max);
            }
            else {
                throw new Error('Cannot calculate roots. Symbol must be a polynomial!');
            }

            function calcroots(rarr, powers, max){	
                var MAXDEGREE = 100; // Degree of largest polynomial accepted by this script.

                // Make a clone of the coefficients before appending the max power
                var p = rarr.slice(0);

                // Divide the string up into its individual entries, which--presumably--are separated by whitespace
                rarr.unshift(max);

                if (max > MAXDEGREE){
                    throw new Error("This utility accepts polynomials of degree up to " + MAXDEGREE + ". ");
                }

                var zeroi = [],   // Vector of imaginary components of roots
                    degreePar = {};    // degreePar is a dummy variable for passing the parameter POLYDEGREE by reference
                degreePar.Degree = max; 

                for (i = 0; i < max; i++) {
                    zeroi.push(0);
                }
                var zeror = zeroi.slice(0); // Vector of real components of roots

                // Find the roots
                //--> Begin Jenkins-Traub

                /*
                 * A verbatim copy of Mr. David Binner's Jenkins-Traub port
                */
               function QuadSD_ak1(NN, u, v, p, q, iPar){
                   // Divides p by the quadratic 1, u, v placing the quotient in q and the remainder in a, b
                   // iPar is a dummy variable for passing in the two parameters--a and b--by reference
                   q[0] = iPar.b = p[0];
                   q[1] = iPar.a = -(u*iPar.b) + p[1];

                   for (var i = 2; i < NN; i++){
                       q[i] = -(u*iPar.a + v*iPar.b) + p[i];
                       iPar.b = iPar.a;
                       iPar.a = q[i];
                   } 
                   return;
               } 

               function calcSC_ak1(DBL_EPSILON, N, a, b, iPar, K, u, v, qk){
                   // This routine calculates scalar quantities used to compute the next K polynomial and
                   // new estimates of the quadratic coefficients.
                   // calcSC -	integer variable set here indicating how the calculations are normalized
                   // to avoid overflow.
                   // iPar is a dummy variable for passing in the nine parameters--a1, a3, a7, c, d, e, f, g, and h --by reference

                   // sdPar is a dummy variable for passing the two parameters--c and d--into QuadSD_ak1 by reference
                   var sdPar = new Object(),    
                   // TYPE = 3 indicates the quadratic is almost a factor of K
                       dumFlag = 3;	

                   // Synthetic division of K by the quadratic 1, u, v
                   sdPar.b =  sdPar.a = 0.0;
                   QuadSD_ak1(N, u, v, K, qk, sdPar);
                   iPar.c = sdPar.a;
                   iPar.d = sdPar.b;

                   if (Math.abs(iPar.c) <= (100.0*DBL_EPSILON*Math.abs(K[N - 1]))) {
                       if (Math.abs(iPar.d) <= (100.0*DBL_EPSILON*Math.abs(K[N - 2])))  return dumFlag;
                   } 

                   iPar.h = v*b;
                   if (Math.abs(iPar.d) >= Math.abs(iPar.c)){
                         // TYPE = 2 indicates that all formulas are divided by d
                       dumFlag = 2;		
                       iPar.e = a/(iPar.d);
                       iPar.f = (iPar.c)/(iPar.d);
                       iPar.g = u*b;
                       iPar.a3 = (iPar.e)*((iPar.g) + a) + (iPar.h)*(b/(iPar.d));
                       iPar.a1 = -a + (iPar.f)*b;
                       iPar.a7 = (iPar.h) + ((iPar.f) + u)*a;
                   } 
                   else {
                       // TYPE = 1 indicates that all formulas are divided by c;
                       dumFlag = 1;		
                       iPar.e = a/(iPar.c);
                       iPar.f = (iPar.d)/(iPar.c);
                       iPar.g = (iPar.e)*u;
                       iPar.a3 = (iPar.e)*a + ((iPar.g) + (iPar.h)/(iPar.c))*b;
                       iPar.a1 = -(a*((iPar.d)/(iPar.c))) + b;
                       iPar.a7 = (iPar.g)*(iPar.d) + (iPar.h)*(iPar.f) + a;
                   } 
                   return dumFlag;
               } 

               function nextK_ak1(DBL_EPSILON, N, tFlag, a, b, iPar, K, qk, qp){
                   // Computes the next K polynomials using the scalars computed in calcSC_ak1
                   // iPar is a dummy variable for passing in three parameters--a1, a3, and a7
                   var temp;
                   if (tFlag == 3){	// Use unscaled form of the recurrence
                       K[1] = K[0] = 0.0;
                       for (var i = 2; i < N; i++)	 { K[i] = qk[i - 2]; }
                       return;
                   } 

                   temp = ((tFlag == 1) ? b : a);
                   if (Math.abs(iPar.a1) > (10.0*DBL_EPSILON*Math.abs(temp))){
                       // Use scaled form of the recurrence
                       iPar.a7 /= iPar.a1;
                       iPar.a3 /= iPar.a1;
                       K[0] = qp[0];
                       K[1] = -(qp[0]*iPar.a7) + qp[1];
                       for (var i = 2; i < N; i++)	 K[i] = -(qp[i - 1]*iPar.a7) + qk[i - 2]*iPar.a3 + qp[i];
                   } 
                   else {
                       // If a1 is nearly zero, then use a special form of the recurrence
                       K[0] = 0.0;
                       K[1] = -(qp[0]*iPar.a7);
                       for (var i = 2; i < N; i++) { K[i] = -(qp[i - 1]*iPar.a7) + qk[i - 2]*iPar.a3; }
                   } 
                   return;
               }

               function newest_ak1(tFlag, iPar, a, a1, a3, a7, b, c, d, f, g, h, u, v, K, N, p){
                   // Compute new estimates of the quadratic coefficients using the scalars computed in calcSC_ak1
                   // iPar is a dummy variable for passing in the two parameters--uu and vv--by reference
                   // iPar.a = uu, iPar.b = vv

                   var a4, a5, b1, b2, c1, c2, c3, c4, temp;
                   iPar.b = iPar.a = 0.0;// The quadratic is zeroed

                   if (tFlag != 3){
                       if (tFlag != 2){
                           a4 = a + u*b + h*f;
                           a5 = c + (u + v*f)*d;
                       } 
                       else { 
                           a4 = (a + g)*f + h;
                           a5 = (f + u)*c + v*d;
                       } 

                       // Evaluate new quadratic coefficients
                       b1 = -(K[N - 1]/p[N]);
                       b2 = -(K[N - 2] + b1*p[N - 1])/p[N];
                       c1 = v*b2*a1;
                       c2 = b1*a7;
                       c3 = b1*b1*a3;
                       c4 = -(c2 + c3) + c1;
                       temp = -c4 + a5 + b1*a4;
                       if (temp != 0.0) {
                           iPar.a = -((u*(c3 + c2) + v*(b1*a1 + b2*a7))/temp) + u;
                           iPar.b = v*(1.0 + c4/temp);
                       } 
                   } 
                   return;
               } 

               function Quad_ak1(a, b1, c, iPar){
                   // Calculates the zeros of the quadratic a*Z^2 + b1*Z + c
                   // The quadratic formula, modified to avoid overflow, is used to find the larger zero if the
                   // zeros are real and both zeros are complex. The smaller real zero is found directly from
                   // the product of the zeros c/a.

                   // iPar is a dummy variable for passing in the four parameters--sr, si, lr, and li--by reference

                   var b, d, e;
                   iPar.sr = iPar.si = iPar.lr = iPar.li = 0.0;

                   if (a == 0) {
                       iPar.sr = ((b1 != 0) ? -(c/b1) : iPar.sr);
                       return;
                   } 
                   if (c == 0){
                       iPar.lr = -(b1/a);
                       return;
                   } 

                   // Compute discriminant avoiding overflow
                   b = b1/2.0;
                   if (Math.abs(b) < Math.abs(c)){
                       e = ((c >= 0) ? a : -a);
                       e = -e + b*(b/Math.abs(c));
                       d = Math.sqrt(Math.abs(e))*Math.sqrt(Math.abs(c));
                   } 
                   else { 
                       e = -((a/b)*(c/b)) + 1.0;
                       d = Math.sqrt(Math.abs(e))*(Math.abs(b));
                   } 

                   if (e >= 0) {
                       // Real zeros
                       d = ((b >= 0) ? -d : d);
                       iPar.lr = (-b + d)/a;
                       iPar.sr = ((iPar.lr != 0) ? (c/(iPar.lr))/a : iPar.sr);
                   }
                   else { 
                       // Complex conjugate zeros
                       iPar.lr = iPar.sr = -(b/a);
                       iPar.si = Math.abs(d/a);
                       iPar.li = -(iPar.si);
                   } 
                   return;
               }  

               function QuadIT_ak1(DBL_EPSILON, N, iPar, uu, vv, qp, NN, sdPar, p, qk, calcPar, K){
                   // Variable-shift K-polynomial iteration for a quadratic factor converges only if the
                   // zeros are equimodular or nearly so.
                   // iPar is a dummy variable for passing in the five parameters--NZ, lzi, lzr, szi, and szr--by reference
                   // sdPar is a dummy variable for passing the two parameters--a and b--in by reference
                   // calcPar is a dummy variable for passing the nine parameters--a1, a3, a7, c, d, e, f, g, and h --in by reference

                   // qPar is a dummy variable for passing the four parameters--szr, szi, lzr, and lzi--into Quad_ak1 by reference
                   var qPar = new Object(),    
                       ee, mp, omp, relstp, t, u, ui, v, vi, zm,
                       i, j = 0, tFlag, triedFlag = 0;   // Integer variables

                   iPar.NZ = 0;// Number of zeros found
                   u = uu; // uu and vv are coefficients of the starting quadratic
                   v = vv;

                   do {
                       qPar.li = qPar.lr =  qPar.si = qPar.sr = 0.0;
                       Quad_ak1(1.0, u, v, qPar);
                       iPar.szr = qPar.sr;
                       iPar.szi = qPar.si;
                       iPar.lzr = qPar.lr;
                       iPar.lzi = qPar.li;

                       // Return if roots of the quadratic are real and not close to multiple or nearly
                       // equal and of opposite sign.
                       if (Math.abs(Math.abs(iPar.szr) - Math.abs(iPar.lzr)) > 0.01*Math.abs(iPar.lzr))  break;

                       // Evaluate polynomial by quadratic synthetic division

                       QuadSD_ak1(NN, u, v, p, qp, sdPar);

                       mp = Math.abs(-((iPar.szr)*(sdPar.b)) + (sdPar.a)) + Math.abs((iPar.szi)*(sdPar.b));

                       // Compute a rigorous bound on the rounding error in evaluating p

                       zm = Math.sqrt(Math.abs(v));
                       ee = 2.0*Math.abs(qp[0]);
                       t = -((iPar.szr)*(sdPar.b));

                       for (i = 1; i < N; i++)  { ee = ee*zm + Math.abs(qp[i]); }

                       ee = ee*zm + Math.abs(t + sdPar.a);
                       ee = (9.0*ee + 2.0*Math.abs(t) - 7.0*(Math.abs((sdPar.a) + t) + zm*Math.abs((sdPar.b))))*DBL_EPSILON;

                       // Iteration has converged sufficiently if the polynomial value is less than 20 times this bound
                       if (mp <= 20.0*ee){
                           iPar.NZ = 2;
                           break;
                       } 

                       j++;
                       // Stop iteration after 20 steps
                       if (j > 20)  break;
                       if (j >= 2){
                           if ((relstp <= 0.01) && (mp >= omp) && (!triedFlag)){
                               // A cluster appears to be stalling the convergence. Five fixed shift
                               // steps are taken with a u, v close to the cluster.
                               relstp = ((relstp < DBL_EPSILON) ? Math.sqrt(DBL_EPSILON) : Math.sqrt(relstp));
                               u -= u*relstp;
                               v += v*relstp;

                               QuadSD_ak1(NN, u, v, p, qp, sdPar);
                               for (i = 0; i < 5; i++){
                                   tFlag = calcSC_ak1(DBL_EPSILON, N, sdPar.a, sdPar.b, calcPar, K, u, v, qk);
                                   nextK_ak1(DBL_EPSILON, N, tFlag, sdPar.a, sdPar.b, calcPar, K, qk, qp);
                               } 

                               triedFlag = 1;
                               j = 0;

                           } 
                       }
                       omp = mp;

                       // Calculate next K polynomial and new u and v
                       tFlag = calcSC_ak1(DBL_EPSILON, N, sdPar.a, sdPar.b, calcPar, K, u, v, qk);
                       nextK_ak1(DBL_EPSILON, N, tFlag, sdPar.a, sdPar.b, calcPar, K, qk, qp);
                       tFlag = calcSC_ak1(DBL_EPSILON, N, sdPar.a, sdPar.b, calcPar, K, u, v, qk);
                       newest_ak1(tFlag, sdPar, sdPar.a, calcPar.a1, calcPar.a3, calcPar.a7, sdPar.b, calcPar.c, calcPar.d, calcPar.f, calcPar.g, calcPar.h, u, v, K, N, p);
                       ui = sdPar.a;
                       vi = sdPar.b;

                       // If vi is zero, the iteration is not converging
                       if (vi != 0){
                           relstp = Math.abs((-v + vi)/vi);
                           u = ui;
                           v = vi;
                       } 
                   } while (vi != 0); 
                   return;
               } 

               function RealIT_ak1(DBL_EPSILON, iPar, sdPar, N, p, NN, qp, K, qk){
                   // Variable-shift H-polynomial iteration for a real zero
                   // sss	- starting iterate = sdPar.a
                   // NZ		- number of zeros found = iPar.NZ
                   // dumFlag	- flag to indicate a pair of zeros near real axis, returned to iFlag

                   var ee, kv, mp, ms, omp, pv, s, t,
                       dumFlag, i, j, nm1 = N - 1;   // Integer variables

                   iPar.NZ = j = dumFlag = 0;
                   s = sdPar.a;

                   for ( ; ; ) {
                       pv = p[0];

                       // Evaluate p at s
                       qp[0] = pv;
                       for (i = 1; i < NN; i++)  { qp[i] = pv = pv*s + p[i]; }
                       mp = Math.abs(pv);

                       // Compute a rigorous bound on the error in evaluating p
                       ms = Math.abs(s);
                       ee = 0.5*Math.abs(qp[0]);
                       for (i = 1; i < NN; i++)  { ee = ee*ms + Math.abs(qp[i]); }

                       // Iteration has converged sufficiently if the polynomial value is less than
                       // 20 times this bound
                       if (mp <= 20.0*DBL_EPSILON*(2.0*ee - mp)){
                           iPar.NZ = 1;
                           iPar.szr = s;
                           iPar.szi = 0.0;
                           break;
                       } 
                       j++;
                       // Stop iteration after 10 steps
                       if (j > 10)  break;

                       if (j >= 2){
                           if ((Math.abs(t) <= 0.001*Math.abs(-t + s)) && (mp > omp)){
                               // A cluster of zeros near the real axis has been encountered.
                               // Return with iFlag set to initiate a quadratic iteration.
                               dumFlag = 1;
                               iPar.a = s;
                               break;
                           } // End if ((fabs(t) <= 0.001*fabs(s - t)) && (mp > omp))
                       } //End if (j >= 2)

                       // Return if the polynomial value has increased significantly
                       omp = mp;

                       // Compute t, the next polynomial and the new iterate
                       qk[0] = kv = K[0];
                       for (i = 1; i < N; i++)	 { qk[i] = kv = kv*s + K[i]; }

                       if (Math.abs(kv) > Math.abs(K[nm1])*10.0*DBL_EPSILON){
                           // Use the scaled form of the recurrence if the value of K at s is non-zero
                           t = -(pv/kv);
                           K[0] = qp[0];
                           for (i = 1; i < N; i++) { K[i] = t*qk[i - 1] + qp[i]; }
                       }
                       else { 
                           // Use unscaled form
                           K[0] = 0.0;
                           for (i = 1; i < N; i++)	 K[i] = qk[i - 1];
                       }

                       kv = K[0];
                       for (i = 1; i < N; i++) { kv = kv*s + K[i]; }
                       t = ((Math.abs(kv) > (Math.abs(K[nm1])*10.0*DBL_EPSILON)) ? -(pv/kv) : 0.0);
                       s += t;
                   } 
                   return dumFlag;
               } 

               function Fxshfr_ak1(DBL_EPSILON, MDP1, L2, sr, v, K, N, p, NN, qp, u, iPar){

                   // Computes up to L2 fixed shift K-polynomials, testing for convergence in the linear or
                   // quadratic case. Initiates one of the variable shift iterations and returns with the
                   // number of zeros found.
                   // L2	limit of fixed shift steps
                   // iPar is a dummy variable for passing in the five parameters--NZ, lzi, lzr, szi, and szr--by reference
                   // NZ	number of zeros found
                   var sdPar = new Object(),    // sdPar is a dummy variable for passing the two parameters--a and b--into QuadSD_ak1 by reference
                       calcPar = new Object(),
                       // calcPar is a dummy variable for passing the nine parameters--a1, a3, a7, c, d, e, f, g, and h --into calcSC_ak1 by reference

                       qk = new Array(MDP1),
                       svk = new Array(MDP1),
                       a, b, betas, betav, oss, ots, otv, ovv, s, ss, ts, tss, tv, tvv, ui, vi, vv,
                       fflag, i, iFlag = 1, j, spass, stry, tFlag, vpass, vtry;     // Integer variables

                   iPar.NZ = 0;
                   betav = betas = 0.25;
                   oss = sr;
                   ovv = v;

                   //Evaluate polynomial by synthetic division
                   sdPar.b =  sdPar.a = 0.0;
                   QuadSD_ak1(NN, u, v, p, qp, sdPar);
                   a = sdPar.a;
                   b = sdPar.b;
                   calcPar.h = calcPar.g = calcPar.f = calcPar.e = calcPar.d = calcPar.c = calcPar.a7 = calcPar.a3 = calcPar.a1 = 0.0;
                   tFlag = calcSC_ak1(DBL_EPSILON, N, a, b, calcPar, K, u, v, qk);

                   for (j = 0; j < L2; j++){
                       fflag = 1;

                       // Calculate next K polynomial and estimate v
                       nextK_ak1(DBL_EPSILON, N, tFlag, a, b, calcPar, K, qk, qp);
                       tFlag = calcSC_ak1(DBL_EPSILON, N, a, b, calcPar, K, u, v, qk);

                       // Use sdPar for passing in uu and vv instead of defining a brand-new variable.
                       // sdPar.a = ui, sdPar.b = vi
                       newest_ak1(tFlag, sdPar, a, calcPar.a1, calcPar.a3, calcPar.a7, b, calcPar.c, calcPar.d, calcPar.f, calcPar.g, calcPar.h, u, v, K, N, p);
                       ui = sdPar.a;
                       vv = vi = sdPar.b;

                       // Estimate s
                       ss = ((K[N - 1] != 0.0) ? -(p[N]/K[N - 1]) : 0.0);
                       ts = tv = 1.0;

                       if ((j != 0) && (tFlag != 3)){
                           // Compute relative measures of convergence of s and v sequences
                           tv = ((vv != 0.0) ? Math.abs((vv - ovv)/vv) : tv);
                           ts = ((ss != 0.0) ? Math.abs((ss - oss)/ss) : ts);

                           // If decreasing, multiply the two most recent convergence measures
                           tvv = ((tv < otv) ? tv*otv : 1.0);
                           tss = ((ts < ots) ? ts*ots : 1.0);

                           // Compare with convergence criteria
                           vpass = ((tvv < betav) ? 1 : 0);
                           spass = ((tss < betas) ? 1 : 0);

                           if ((spass) || (vpass)){

                               // At least one sequence has passed the convergence test.
                               // Store variables before iterating

                               for (i = 0; i < N; i++) { svk[i] = K[i]; }
                               s = ss;

                               // Choose iteration according to the fastest converging sequence

                                 stry = vtry = 0;

                               for ( ; ; ) {
                                   if ((fflag && ((fflag = 0) == 0)) && ((spass) && (!vpass || (tss < tvv)))){
                                       ;// Do nothing. Provides a quick "short circuit".
                                   } 
                                   else { 
                                       QuadIT_ak1(DBL_EPSILON, N, iPar, ui, vi, qp, NN, sdPar, p, qk, calcPar, K);
                                       a = sdPar.a;
                                       b = sdPar.b;

                                       if ((iPar.NZ) > 0) return;

                                       // Quadratic iteration has failed. Flag that it has been tried and decrease the
                                       // convergence criterion
                                       iFlag = vtry = 1;
                                       betav *= 0.25;

                                       // Try linear iteration if it has not been tried and the s sequence is converging
                                       if (stry || (!spass)){
                                           iFlag = 0;
                                       }
                                       else {
                                           for (i = 0; i < N; i++) K[i] = svk[i];
                                       } 
                                   }
                                   //fflag = 0;
                                   if (iFlag != 0){
                                       // Use sdPar for passing in s instead of defining a brand-new variable.
                                       // sdPar.a = s
                                       sdPar.a = s;
                                       iFlag = RealIT_ak1(DBL_EPSILON, iPar, sdPar, N, p, NN, qp, K, qk);
                                       s = sdPar.a;

                                       if ((iPar.NZ) > 0) return;

                                       // Linear iteration has failed. Flag that it has been tried and decrease the
                                       // convergence criterion
                                       stry = 1;
                                       betas *= 0.25;

                                       if (iFlag != 0){
                                           // If linear iteration signals an almost double real zero, attempt quadratic iteration
                                           ui = -(s + s);
                                           vi = s*s;
                                           continue;

                                       } 
                                   } 

                                   // Restore variables
                                   for (i = 0; i < N; i++) K[i] = svk[i];

                                   // Try quadratic iteration if it has not been tried and the v sequence is converging
                                   if (!vpass || vtry) break;		// Break out of infinite for loop

                               } 

                               // Re-compute qp and scalar values to continue the second stage

                               QuadSD_ak1(NN, u, v, p, qp, sdPar);
                               a = sdPar.a;
                               b = sdPar.b;

                               tFlag = calcSC_ak1(DBL_EPSILON, N, a, b, calcPar, K, u, v, qk);
                           } 
                       } 
                       ovv = vv;
                       oss = ss;
                       otv = tv;
                       ots = ts;
                   } 
                   return;
               }  

               function rpSolve(degPar, p, zeror, zeroi){ 
                   var N = degPar.Degree,
                       RADFAC = 3.14159265358979323846/180,  // Degrees-to-radians conversion factor = PI/180
                       LB2 = Math.LN2,// Dummy variable to avoid re-calculating this value in loop below
                       MDP1 = degPar.Degree + 1,
                       K = new Array(MDP1),
                       pt = new Array(MDP1),
                       qp = new Array(MDP1),
                       temp = new Array(MDP1),
                       // qPar is a dummy variable for passing the four parameters--sr, si, lr, and li--by reference
                       qPar = new Object(),
                       // Fxshfr_Par is a dummy variable for passing parameters by reference : NZ, lzi, lzr, szi, szr);
                       Fxshfr_Par = new Object(),
                       bnd, DBL_EPSILON, df, dx, factor, ff, moduli_max, moduli_min, sc, x, xm,
                       aa, bb, cc, sr, t, u, xxx,
                       j, jj, l, NM1, NN, zerok;// Integer variables

                   // Calculate the machine epsilon and store in the variable DBL_EPSILON.
                   // To calculate this value, just use existing variables rather than create new ones that will be used only for this code block
                   aa = 1.0;
                   do {
                       DBL_EPSILON = aa;
                       aa /= 2;
                       bb = 1.0 + aa;
                   } while (bb > 1.0);

                   var LO = Number.MIN_VALUE/DBL_EPSILON,
                       cosr = Math.cos(94.0*RADFAC),// = -0.069756474
                       sinr = Math.sin(94.0*RADFAC),// = 0.99756405
                       xx = Math.sqrt(0.5),// = 0.70710678
                       yy = -xx;

                   Fxshfr_Par.NZ = j = 0;
                   Fxshfr_Par.szr = Fxshfr_Par.szi =  Fxshfr_Par.lzr = Fxshfr_Par.lzi = 0.0;

                   // Remove zeros at the origin, if any
                   while (p[N] == 0){
                       zeror[j] = zeroi[j] = 0;
                       N--;
                       j++;
                   }
                   NN = N + 1;

                   // >>>>> Begin Main Loop <<<<<
                   while (N >= 1){ // Main loop
                       // Start the algorithm for one zero
                       if (N <= 2){
                           // Calculate the final zero or pair of zeros
                           if (N < 2){
                               zeror[degPar.Degree - 1] = -(p[1]/p[0]);
                               zeroi[degPar.Degree - 1] = 0;
                           } 
                           else { 
                               qPar.li = qPar.lr =  qPar.si = qPar.sr = 0.0;
                               Quad_ak1(p[0], p[1], p[2], qPar);
                               zeror[degPar.Degree - 2] = qPar.sr;
                               zeroi[degPar.Degree - 2] = qPar.si;
                               zeror[degPar.Degree - 1] = qPar.lr;
                               zeroi[degPar.Degree - 1] = qPar.li;
                           } 
                             break;
                       } 

                       // Find the largest and smallest moduli of the coefficients
                       moduli_max = 0.0;
                       moduli_min = Number.MAX_VALUE;

                       for (i = 0; i < NN; i++){
                           x = Math.abs(p[i]);
                           if (x > moduli_max) moduli_max = x;
                           if ((x != 0) && (x < moduli_min)) moduli_min = x;
                       }

                       // Scale if there are large or very small coefficients
                       // Computes a scale factor to multiply the coefficients of the polynomial. The scaling
                       // is done to avoid overflow and to avoid undetected underflow interfering with the
                       // convergence criterion.
                       // The factor is a power of the base.
                       sc = LO/moduli_min;

                       if (((sc <= 1.0) && (moduli_max >= 10)) || ((sc > 1.0) && (Number.MAX_VALUE/sc >= moduli_max))){
                           sc = ((sc == 0) ? Number.MIN_VALUE : sc);
                           l = Math.floor(Math.log(sc)/LB2 + 0.5);
                           factor = Math.pow(2.0, l);
                           if (factor != 1.0){
                               for (i = 0; i < NN; i++) p[i] *= factor;
                           } 
                       } 

                       // Compute lower bound on moduli of zeros
                       for (var i = 0; i < NN; i++) pt[i] = Math.abs(p[i]);
                       pt[N] = -(pt[N]);
                       NM1 = N - 1;

                       // Compute upper estimate of bound
                       x = Math.exp((Math.log(-pt[N]) - Math.log(pt[0]))/N);

                       if (pt[NM1] != 0) {
                           // If Newton step at the origin is better, use it
                           xm = -pt[N]/pt[NM1];
                           x = ((xm < x) ? xm : x);
                       } 

                       // Chop the interval (0, x) until ff <= 0
                       xm = x;
                       do {
                           x = xm;
                           xm = 0.1*x;
                           ff = pt[0];
                           for (var i = 1; i < NN; i++) { ff = ff *xm + pt[i]; }
                       } while (ff > 0); // End do-while loop

                       dx = x;
                       // Do Newton iteration until x converges to two decimal places

                       do {
                           df = ff = pt[0];
                           for (var i = 1; i < N; i++){
                               ff = x*ff + pt[i];
                               df = x*df + ff;
                           } // End for i
                           ff = x*ff + pt[N];
                           dx = ff/df;
                           x -= dx;
                       } while (Math.abs(dx/x) > 0.005); // End do-while loop

                       bnd = x;

                       // Compute the derivative as the initial K polynomial and do 5 steps with no shift
                       for (var i = 1; i < N; i++) K[i] = (N - i)*p[i]/N;
                       K[0] = p[0];
                       aa = p[N];
                       bb = p[NM1];
                       zerok = ((K[NM1] == 0) ? 1 : 0);

                       for (jj = 0; jj < 5; jj++) {
                           cc = K[NM1];
                               if (zerok){
                                   // Use unscaled form of recurrence
                                   for (var i = 0; i < NM1; i++){
                                       j = NM1 - i;
                                       K[j] = K[j - 1];
                                   } // End for i
                                   K[0] = 0;
                                   zerok = ((K[NM1] == 0) ? 1 : 0);
                               } 
                               else { 
                                   // Used scaled form of recurrence if value of K at 0 is nonzero
                                   t = -aa/cc;
                                   for (var i = 0; i < NM1; i++){
                                       j = NM1 - i;
                                       K[j] = t*K[j - 1] + p[j];
                                   } // End for i
                                   K[0] = p[0];
                                   zerok = ((Math.abs(K[NM1]) <= Math.abs(bb)*DBL_EPSILON*10.0) ? 1 : 0);
                               } 
                       } 

                       // Save K for restarts with new shifts
                       for (var i = 0; i < N; i++) temp[i] = K[i];

                       // Loop to select the quadratic corresponding to each new shift
                       for (jj = 1; jj <= 20; jj++){

                           // Quadratic corresponds to a double shift to a non-real point and its
                           // complex conjugate. The point has modulus BND and amplitude rotated
                           // by 94 degrees from the previous shift.

                           xxx = -(sinr*yy) + cosr*xx;
                           yy = sinr*xx + cosr*yy;
                           xx = xxx;
                           sr = bnd*xx;
                           u = -(2.0*sr);

                           // Second stage calculation, fixed quadratic
                           Fxshfr_ak1(DBL_EPSILON, MDP1, 20*jj, sr, bnd, K, N, p, NN, qp, u, Fxshfr_Par);

                           if (Fxshfr_Par.NZ != 0){
                               // The second stage jumps directly to one of the third stage iterations and
                               // returns here if successful. Deflate the polynomial, store the zero or
                               // zeros, and return to the main algorithm.
                               j = degPar.Degree - N;
                               zeror[j] = Fxshfr_Par.szr;
                               zeroi[j] = Fxshfr_Par.szi;
                               NN = NN - Fxshfr_Par.NZ;
                               N = NN - 1;
                               for (var i = 0; i < NN; i++) p[i] = qp[i];
                               if (Fxshfr_Par.NZ != 1){
                                   zeror[j + 1] = Fxshfr_Par.lzr;
                                   zeroi[j + 1] = Fxshfr_Par.lzi;
                               }
                               break;
                           } 
                           else { 
                             // If the iteration is unsuccessful, another quadratic is chosen after restoring K
                             for (var i = 0; i < N; i++) { K[i] = temp[i]; }
                           } 
                       } 
                       // Return with failure if no convergence with 20 shifts
                       if (jj > 20) {
                           degPar.Degree -= N;
                           break;
                       } 
                   }
                   // >>>>> End Main Loop <<<<<
                   return;
               }
                //--> End Jenkins-Traub
                rpSolve(degreePar, p, zeror, zeroi);

                var l = zeroi.length;
                //format the output
                for( i=0; i<l; i++ ) {
                    // We round the imaginary part to avoid having something crazy like 5.67e-16.
                    var img = round( zeroi[i], decp+8 ),
                        real = round( zeror[i], decp );
                    // Did the rounding pay off? If the rounding did nothing more than chop off a few digits then no.
                    // If the rounding results in a a number at least 3 digits shorter we'll keep it else we'll keep 
                    // the original otherwise the rounding was worth it.
                    real = decp - String( real ).length > 2 ? real : zeror[i];
                    var sign = img < 0 ? '-' : '';

                    // Remove the zeroes
                    if( real === 0 ) { real = ''; }
                    if( img === 0 ) { img = ''; }

                    // Remove 1 as the multiplier and discard imaginary part if there isn't one.
                    img = Math.abs( img ) === 1 ? sign+'i' : ( img ? img+'*i' : '' );

                    var num = ( real && img ) ? real + '+' + img : real+img;
                    zeror[i] = num.replace(/\+\-/g, '-');
                }
                return zeror;
            } 
         },
        froot: function(f, guess, dx) { 
            var newtonraph = function(xn) {
                var mesh = 1e-12,
                    // If the derivative was already provided then don't recalculate.
                    df = dx ? dx : core.Utils.build(core.Calculus.diff(f.clone())),
                    
                    // If the function was passed in as a function then don't recalculate.
                    fn = f instanceof Function ? f : core.Utils.build(f),
                    max = 10000,
                    done = false, 
                    safety = 0;
                while( !done ) { 
                    var x = xn-(fn(xn)/df(xn));
                    //absolute values for both x & xn ensures that we indeed have the radius    
                    var r = Math.abs(x) - Math.abs(xn),
                        delta = Math.abs(r);
                    xn = x; 

                    if( delta < mesh ) done = true;
                    else if( safety > max ) {
                        xn = null;
                        done = true;
                    }
                    
                    safety++;
                }
                return xn;
            };
            return newtonraph( Number( guess ) );
        },
        /**
         * Standard square free factorization 
         * @param {Polynomial} a
         * @returns {Array}
         */
        sqfr: function(a) { 
            var i = 1;
            var b = a.clone().diff(); 
            var c = a.clone().gcd(b);
            var w = a.divide(c)[0],
                output = Polynomial.fromArray([1], a.variable);
            while(!c.equalsNumber(1)) {
                var y = w.gcd(c); 
                var z = w.divide(y)[0];
                output = output.multiply(z); 
                i++;
                w = y;
                c = c.divide(y)[0];
            }
            return [output, w, i];
        },
        quad: function(a, b, c) {
            var q = function(a, b, c, sign) {
                return _.parse('-('+b+'+'+sign+'*sqrt(('+b+')^2-4*('+a+')*('+c+')))/(2*'+a+')');
            };
            return [q(a, b, c, 1), q(a, b, c, -1)];
        },
        sumProd: function(a, b) {
            return __.quad(-b, a, -1).map(function(x){
                return x.invert(); 
            });
        },
        /**
         * Get's all the powers of a particular polynomial including the denominators. The denominators powers
         * are returned as negative. All remaining polynomials are returned as zero order polynomials.
         * for example polyPowers(x^2+1/x+y+t) will return [ '-1', 0, '2' ]
         * @param {Symbol} e
         * @param {String} for_variable
         * @param {Array} powers
         * @returns {Array} An array of the powers
         */
        //assumes you've already verified that it's a polynomial
        polyPowers: function(e, for_variable, powers) { 
            powers = powers || [];
            var g = g = e.group; 
            if(g ===  PL && for_variable === e.value) {
                powers = powers.concat(keys(e.symbols)); 
            }
            else if(g === CP) { 
                for(var s in e.symbols) {
                    var symbol = e.symbols[s]; 
                    var g = symbol.group, v = symbol.value; 
                    if(g === S && for_variable === v) powers.push(symbol.power);
                    else if(g === PL || g === CP) powers = __.polyPowers(symbol, for_variable, powers);
                    else if(g === CB && symbol.contains(for_variable)) {
                        var t = symbol.symbols[for_variable];
                        if(t) powers.push((t.power));
                    }
                    else if(g === N || for_variable !== v) powers.push(0);
                }
            }
            return core.Utils.arrayUnique(powers).sort();
        },
        /**
         * WARNING: THIS IS NOT A PROPER FACTORING ALGORITHM. JUST A QUICK FIX
         * http://www.ams.org/journals/mcom/1978-32-144/S0025-5718-1978-0568284-3/S0025-5718-1978-0568284-3.pdf
         * Splits symbol into factors
         * @param {Symbol} symbol
         * @returns {Symbol}
         */
        factor: function(symbol) {
            var retval = symbol,
                group = symbol.group,
                isCompositionGroup = function(group) {
                    return (group === PL || group === CP);
                };

            if(isCompositionGroup(group)) {
                //distribute the multiplier in sub-symbols
                for(var x in symbol.symbols) symbol.symbols[x].distributeMultiplier(); 
                //factor the multiplier
                var gcf = core.Math2.GCD.apply(undefined, symbol.coeffs()),
                       
                    factorize = function(symbol) { 
                        for(var x in symbol.symbols) {
                            var sub = symbol.symbols[x]; 
                            if(isCompositionGroup(sub.group)) {
                                factorize(sub);
                            }
                            else {
                                sub.multiplier /= gcf;
                            }
                        }
                    };
                
                if(symbol.power <= 1) {
                    factorize(symbol);
                    symbol.multiplier *= Math.pow(gcf, symbol.power);

                    if(group === PL) {
                        var powers = keys(symbol.symbols),
                            lowest_power = core.Utils.arrayMin(powers),
                            factor = _.parse(symbol.value+'^'+lowest_power);
                        var factored = new core.Symbol(0);
                        for(var x in symbol.symbols) {
                            factored = _.add(factored, _.divide(symbol.symbols[x], factor.clone()));
                        }

                        factored = _.symfunction(core.PARENTHESIS, [factored]);//place it parenthesis
                        factored.power *= symbol.power;
                        factored.multiplier *= symbol.multiplier;
                        factor.power *= symbol.power;

                        retval = _.multiply(factor, factored);
                    }
                    else if(group === CP) { 
                        try{
                            var p = symbol.power,
                                roots = core.Algebra.proots(symbol),
                                all_ints = true; 
                            for(var i=0; i<roots.length; i++) {
                                if(!isInt(roots[i])) all_ints = false;
                            }
                            var result = new Symbol(1);
                            if(all_ints)  { 
                                roots.map(function(root) { 
                                    result = _.multiply(result, 
                                        _.symfunction(core.PARENTHESIS, 
                                        [_.subtract(new Symbol(variables(symbol)[0]), new Symbol(root))]));
                                });
                                result.multiplier *= symbol.multiplier;
                                retval = result; 
                            }
                        }
                        catch(e) { 
                            try {
                                //not a polynomial. No biggie. Let's see if we can extract a few variables
                                var symbols = symbol.collectSymbols(),
                                    num_symbols = symbol.length,
                                    hash_table = {};
                                for(var i=0; i<num_symbols; i++) {
                                    var cur_symbol = symbols[i], //collect all the variables contained in the symbol
                                        num_vars = vars.length;
                                    for(var j=0; j<num_vars; j++) {
                                        var var_name = vars[j],
                                            variable = cur_symbol.value === var_name ? cur_symbol : cur_symbol.symbols[var_name],
                                            var_record = hash_table[var_name];
                                        if(isSymbol(variable.power)) throw new Error('Cannot factor symbol. Exiting');
                                        if(!var_record) hash_table[var_name] = [1, variable.power];
                                        else {
                                            var_record[0]++;
                                            var p = variable.power;
                                            if(p < var_record[1]) var_record[1] = p;
                                        }
                                    }
                                }
                                var factor = [];
                                //we now know which variables we have and to which power so we can start reducing
                                for(var x in hash_table) {
                                    var_record = hash_table[x];
                                    //if we have as many recorded as there were sub-symbols then we can divide all of them
                                    //by that symbol
                                    if(var_record[0] === num_symbols) { 
                                        factor.push(x+'^'+var_record[1]);
                                    }
                                };

                                //we can now divide each one by that factor
                                factor = _.parse(factor.join('*'));//make it a Symbol
                                for(x in symbol.symbols) {
                                    symbol.symbols[x] = _.divide(symbol.symbols[x], factor.clone());
                                }

                                retval = _.multiply(_.parse(symbol.text()), factor);
                            }
                            catch(e){;}
                        }
                    }
                }     
            }
            
            if(retval.group === core.groups.FN) retval.updateHash();
            
            return retval;
        },
        /**
         * Checks to see if a set of "equations" is linear. 
         * @param {type} set
         * @returns {Boolean}
         */
        allLinear: function(set) {
            var l = set.length;
            for(var i=0; i<l; i++) if(!__.isLinear(set[i])) return false;
            return true;
        },
        /*
         * Checks to see if the "equation" is linear
         * @param {Symbol} e
         * @returns {boolean}
         */
        isLinear: function(e) {
            var status = false, g = e.group;
            if(g === PL || g === CP) {
                status = true;
                for(var s in e.symbols) {
                    var symbol = e.symbols[s], sg = symbol.group;
                    if(sg === FN || sg === EX || sg === CB) { status = false;}
                    else {
                        if(sg === PL || sg === CP) status = __.isLinear(symbol);
                        else {
                            if(symbol.group !== N && symbol.power.toString() !== '1') { status = false; break; }
                        }
                    }
                }
            }
            else if(g === S && e.power === 1) status = true;
            return status;
        },
        gcd: function(a, b) { 
            a = new Polynomial(a); b = new Polynomial(b);
            return a.gcd(b).toSymbol();
        },
        hasLargerPower: function(a, b, variable) {
            var p1;
            //A very unfortunate side effect of how symbols are stored
            if(a.group === PL) {
                p1 = Math.max(core.Utils.keys(a.symbols));
            }
            else {
                p1 = (a.group === S ? a.power : a.symbols[variable].power).toString();
            }
            //the second symbol might be of group PL
            var p2 = (b.group === S ? b.power : b.symbols[variable].power).toString();
            return p2 > p1;
        },
        /**
         * Divides one expression by another
         * @param {Symbol} symbol1
         * @param {Symbol} symbol2
         * @returns {Array}
         */
        div: function(symbol1, symbol2) {     
            /*
             * * This function follows a similar principle as the Euclidian algorithm by 
             * attempting to reduce one term during each iteration
             * except that it doesn't care about the order. This presents some inefficiency as 
             * extra terms on both sides of the sign are generated. However due to the sign
             * the just end up canceling out.
             * ---------------------------------------------------------------------------------
             * Roughly explained. Given: dividend ==  (symbol1) & divisor == polynomial (symbol2)
             * 1. Find a monomial in the dividend containing the variable which has a power <= to the monomial in the divisor
             *  e.g. x^2*y*z is a suitable divisor for x^2*y^2*z (the order of the variable doesn't matter)
             * 2. Repeat this step for all the terms in the divisor
             * 3. Make sure to mark terms which already have been selected in the dividend
             * 3a. If no suitable match was found for one of the terms then it goes to the remainder
             * 3b. If no suitable match was found for any of the terms in the divisor then we're done
             * 4. Get a q by dividing the first terms in the selected array by its match
             * 5. Multiply all terms in the divisor by q. Call this q_div
             * 6. Subtract q_div from the dividend
             * 7. Repeat 1,2,3,4,5,6 until either 3b is true or dividend = 0
             */
            //enable support for functions by temporarily substituting them for a variable
            var variables = core.Utils.variables,
                vars_a = variables(symbol1), vars_b = variables(symbol2),
                map = {},
                a = _.parse(core.Utils.subFunctions(symbol1, map)),
                b = _.parse(core.Utils.subFunctions(symbol2, map)),
                subs = {};

            //prepare substitutions
            for(var x in map) subs[map[x]] = _.parse(x);

            var al = vars_a.length, bl = vars_b.length, result;
            //quick test to see if we can get ways with univariate division
            if( al <= 1 && bl <= 1 && (vars_a[0] || vars_b[0]) === (vars_b[0] || vars_a[0]) && !symbol1.hasFunc() && !symbol2.hasFunc()) {
                if(al === bl && al === 0) {
                    result = [];
                    var t = a.multiplier.num.divide(a.multiplier.den)
                            .divmod(b.multiplier.num.divide(b.multiplier.den));
                    result[0] = new Symbol(t.quotient);
                    result[1] = new Symbol(t.remainder);
                }
                else {
                    var aa = new Polynomial(a),
                        bb = new Polynomial(b);
                    result = aa.divide(bb).map(function(x) {
                        return x.toSymbol();
                    });
                }
            }
            else { 
                /* check if symbol has denominator */
                //remains constant throughout function
                var divisor = b.collectSymbols(undefined, undefined, Symbol.LSORT, true),
                    quotient = new Symbol(0),
                    remainder = new Symbol(0),
                    dividend = a.collectSymbols(undefined, undefined, Symbol.LSORT, true),
                    divisor_vars = [],
                    ndividend = a.clone();
                    
                //constants at the beginning cause problems. x^0/x^0=x^0 causing no reduction each iteration
                while(divisor[0].isConstant()) divisor.push(divisor.shift());
                
                //cache the variables for the divisor. No need to keep fetching them
                for(var i=0; i<divisor.length; i++) divisor_vars[i] = variables(divisor[i]); 

                while(!ndividend.equals(0)) {
                    var selected = [];
                    var seen = [];
                    //loop through the divisor variables and look for a match
                    for(var i=0; i<divisor_vars.length; i++) {
                        var vars = divisor_vars[i],
                            divisor_term = divisor[i];//store the diversor terms
                        //A constant cannot be the only match to satify division. Since constants are at the end of the list
                        //the if the only match to satisfy division is a constant then just continue;
                        if(selected.length === 0 && divisor_term.isConstant()) continue;
                        
                        var isCB = divisor_term.group === CB;
                        //loop through the variables and look for match
                        for(var j=0; j<dividend.length; j++) {
                            var dividend_term = dividend[j];
                            //Start with default as true since we're going to try and fail the test
                            var select_term = true;
                            var vl = vars.length;
                            //don't bother if it's not a CB since x*y cannot divide y
                            if(isCB && dividend_term.group !== CB) continue;
                            if(seen.indexOf(j) === -1) {
                                for(var k=0; k<vl; k++) {
                                    var variable = vars[k];
                                    if(dividend_term.group === EX || divisor_term.group === EX) break;

                                    if(!dividend_term.contains(variable) || divisor_term.symbols && !dividend_term.symbols) {                                   
                                        select_term = false; //we need to know if this is a good term to use
                                        break; //don't keep looking since this term doesn't satisfy
                                    }
                                    else {
                                        if(__.hasLargerPower(dividend_term, divisor_term, variable)) {
                                            select_term = false;
                                            break;
                                        };
                                    } 
                                }
                                
                                if(select_term) { 
                                    seen.push(j);
                                    selected.push([divisor_term, dividend_term, i]);
                                    break; //we can move to the next set of variables
                                }
                            }   
                        }
                    }


                    //grab a term to use for division. The idea is to knock off one term.
                    //the first one will do just fine
                    var sl = selected.length;
                    
                    if(sl === 0) {
                        //we're done 
                        remainder = _.add(remainder, ndividend);
                        break;
                    }
                    
                    var first_div_term = selected[0];   
                    
                    var q = _.divide(first_div_term[1].clone(), first_div_term[0].clone());
                    
                    //no need to start dipping into the divisor again.
                    if(q.isConstant() && selected.length === 1 && !quotient.hasConstant()) {
                        remainder = ndividend;
                        break;
                    }

                    if(sl < divisor.length) {
                        var idx = first_div_term[2];
                        //handle remainder
                        for(var i=0; i<divisor.length; i++) { 
                            if(i !== idx) {
                                remainder = _.subtract(remainder, _.multiply(q.clone(), divisor[i].clone()));
                            }
                        }
                    }
                    quotient = _.add(quotient, q.clone());
                    var q_div = new Symbol(0);
                    
                    for(var i=0; i<selected.length; i++) {
                        q_div = _.add(q_div, _.multiply(q.clone(), selected[i][0].clone()));
                    }

                    ndividend = _.subtract(ndividend, q_div);

                    if(ndividend.group === CB || ndividend.group === S) dividend = [ndividend];
                    else dividend = ndividend.collectSymbols(undefined, undefined, Symbol.LSORT, true);  

                }
                
                result = [quotient, remainder];
            }
            
            result[0] = _.parse(result[0].text(), subs);
            result[1] = _.parse(result[1].text(), subs);
                
            return result;
        },
        divide: function(symbol1, symbol2) {
            var result = __.div(symbol1, symbol2);
            var remainder = _.divide(result[1], symbol2);
            return _.add(result[0], remainder);
         }
    };
    
    nerdamer.register([
        {
            name: 'factor',
            visible: true,
            numargs: 1,
            build: function() { return __.factor; }
        },
        {
            name: 'gcd',
            visible: true,
            numargs: 2,
            build: function() { return __.gcd; }
        },
        {
            name: 'proots',
            visible: true,
            numargs: -1,
            build: function() { return __.proots; }
        },
        {
            name: 'divide',
            visible: true,
            numargs: 2,
            build: function() { return __.divide; }
        },
        {
            name: 'div',
            visible: true,
            numargs: 2,
            build: function() { return __.div; }
        }
    ]);
})();
