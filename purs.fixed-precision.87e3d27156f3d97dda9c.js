(window.webpackJsonp=window.webpackJsonp||[]).push([[99],{"/+xC":function(n,r,i){"use strict";var t=i("UCB5"),e=i("Z7LG"),u=i("IPld"),o=i("H8k5"),c=i("ZrUp"),a=i("Z24z"),g=i("5QI5"),f=i("zo+L"),m=i("Hnrf"),d=i("sAO9"),s=i("3O1O"),l=i("rkps"),I=i("T4xb"),B=i("NucT"),v=i("j3Ls"),w=i("BCFH"),h=i("UhtL"),b=i("duKJ"),p=i("GFon"),x=function(){function n(){}return n.value=new n,n}(),P=function(n){return n},y=function(n){this.reflectPrecision=n},F=function(n){return function(r){return function(i){var t=b.length(i);return t>=n?b.take(n)(i):i+b.fromCharArray(o.replicate(n-t|0)(r))}}},S=function(n){return n.reflectPrecision},q=function(n){return function(r){var i=S(n)(x.value);return s.round(p.log(c.toNumber(i))/p.ln10)}},R=function(n){return n},z=function(n){return new y(function(r){return w.mul(c.semiringBigInt)(c.fromInt(10))(S(n)(x.value))})},M=new y(function(n){return c.fromInt(1)}),N=new f.Eq(function(n){return function(r){return f.eq(c.eqBigInt)(n)(r)}}),T=new B.Ord(function(){return N},function(n){return function(r){return B.compare(c.ordBigInt)(n)(r)}}),k=function(n){return function(r){return S(n)(x.value)}},C=function(n){return new w.Semiring(function(n){return function(r){return w.add(c.semiringBigInt)(n)(r)}},function(r){return function(i){return m.div(c.euclideanRingBigInt)(w.mul(c.semiringBigInt)(R(r))(R(i)))(k(n)(r))}},S(n)(x.value),w.zero(c.semiringBigInt))},O=function(n){return new v.Ring(function(){return C(n)},function(n){return function(r){return v.sub(c.ringBigInt)(n)(r)}})},E=function(n){return function(r){return function(i){var t,e=q(n)(x.value),u=k(n)(i),a=m.mod(c.euclideanRingBigInt)(i)(u),g=m.div(c.euclideanRingBigInt)(i)(u);return c.toString(g)+I.guard(I.monoidString)(r>0)("."+F(r)("0")((t=e,function(n){return function(r){var i=b.length(r);return i>=t?b.takeRight(t)(r):b.fromCharArray(o.replicate(t-i|0)(n))+r}})("0")(c.toString(a))))}}},A=function(n){return E(n)(q(n)(x.value))};n.exports={fromInt:function(n){return function(r){return w.mul(c.semiringBigInt)(c.fromInt(r))(S(n)(x.value))}},fromNumber:function(n){return function(r){return d.map(l.functorMaybe)(P)(c.fromNumber(r*c.toNumber(S(n)(x.value))))}},toNumber:function(n){return function(r){return c.toNumber(R(r))/c.toNumber(k(n)(r))}},fromString:function(n){return function(r){var i=q(n)(x.value),o=B.between(B.ordChar)("0")("9"),a=b.countPrefix(o)(r),g=S(n)(x.value),m=b.splitAt(a)(r);return e.bind(l.bindMaybe)(c.fromString(m.before))(function(n){return e.bind(l.bindMaybe)(""===m.after?t.pure(l.applicativeMaybe)(w.zero(c.semiringBigInt)):"."===m.after?t.pure(l.applicativeMaybe)(w.zero(c.semiringBigInt)):e.discard(e.discardUnit)(l.bindMaybe)(u.guard(l.monadZeroMaybe)(f.eq(l.eqMaybe(f.eqChar))(b.charAt(0)(m.after))(new l.Just("."))))(function(){var n=b.drop(1)(m.after);return c.fromString(F(i)("0")(n))}))(function(r){return t.pure(l.applicativeMaybe)(w.add(c.semiringBigInt)(w.mul(c.semiringBigInt)(n)(g))(r))})})}},toString:A,toStringWithPrecision:E,numerator:R,floor:function(n){return function(r){var i=k(n)(r),t=m.mod(c.euclideanRingBigInt)(R(r))(i),e=function(){if(B.lessThan(c.ordBigInt)(t)(w.zero(c.semiringBigInt)))return w.add(c.semiringBigInt)(t)(i);if(a.otherwise)return t;throw new Error("Failed pattern match at Data.Fixed (line 227, column 3 - line 228, column 20): "+[])}();return v.sub(c.ringBigInt)(R(r))(e)}},ceil:function(n){return function(r){var i=k(n)(r),t=m.mod(c.euclideanRingBigInt)(R(r))(i),e=function(){if(f.eq(c.eqBigInt)(t)(w.zero(c.semiringBigInt)))return w.zero(c.semiringBigInt);if(B.lessThan(c.ordBigInt)(t)(w.zero(c.semiringBigInt)))return v.negate(c.ringBigInt)(t);if(a.otherwise)return v.sub(c.ringBigInt)(i)(t);throw new Error("Failed pattern match at Data.Fixed (line 251, column 3 - line 253, column 24): "+[])}();return w.add(c.semiringBigInt)(R(r))(e)}},round:function(n){return function(r){var i=k(n)(r),t=m.mod(c.euclideanRingBigInt)(R(r))(i),e=function(){if(B.lessThan(c.ordBigInt)(t)(w.zero(c.semiringBigInt))&&B.greaterThanOrEq(c.ordBigInt)(w.mul(c.semiringBigInt)(w.add(c.semiringBigInt)(t)(i))(c.fromInt(2)))(i))return v.negate(c.ringBigInt)(t);if(B.greaterThanOrEq(c.ordBigInt)(w.mul(c.semiringBigInt)(t)(c.fromInt(2)))(i))return v.sub(c.ringBigInt)(i)(t);if(a.otherwise)return v.negate(c.ringBigInt)(t);throw new Error("Failed pattern match at Data.Fixed (line 278, column 3 - line 280, column 21): "+[])}();return w.add(c.semiringBigInt)(R(r))(e)}},approxDiv:function(n){return function(r){return function(i){var t=R(i),e=R(r),u=k(n)(r);return m.div(c.euclideanRingBigInt)(w.mul(c.semiringBigInt)(e)(u))(t)}}},PProxy:x,KnownPrecision:y,reflectPrecision:S,reflectPrecisionDecimalPlaces:q,reifyPrecision:function(n){return function(r){var i,t=n,e=!1;function u(n,i){return n<0?(e=!0,l.Nothing.value):0===n?(e=!0,new l.Just(i(M)(x.value))):(t=n-1|0,void(r=function(n){return function(r){return i(z(n))(x.value)}}))}for(;!e;)i=u(t,r);return i}},knownPrecisionOne:M,knownPrecisionTenTimes:z,showFixed:function(n){return new h.Show(function(r){return"(fromString "+h.show(h.showString)(A(n)(r))+" :: P"+h.show(c.showBigInt)(S(n)(x.value))+")"})},eqFixed:N,ordFixed:T,semiringFixed:C,ringFixed:O,commutativeRingFixed:function(n){return new g.CommutativeRing(function(){return O(n)})}};i("sygH")}}]);