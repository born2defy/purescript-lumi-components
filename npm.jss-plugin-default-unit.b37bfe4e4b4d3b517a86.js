(window.webpackJsonp=window.webpackJsonp||[]).push([[32],{ctdY:function(t,o,r){"use strict";var i=r("/ceM"),e=i.hasCSSTOMSupport?window.CSS.px:"px",n=i.hasCSSTOMSupport?window.CSS.ms:"ms",a=i.hasCSSTOMSupport?window.CSS.percent:"%";function d(t){var o=/(-[a-z])/g,r=function(t){return t[1].toUpperCase()},i={};for(var e in t)i[e]=t[e],i[e.replace(o,r)]=t[e];return i}var s=d({"animation-delay":n,"animation-duration":n,"background-position":e,"background-position-x":e,"background-position-y":e,"background-size":e,border:e,"border-bottom":e,"border-bottom-left-radius":e,"border-bottom-right-radius":e,"border-bottom-width":e,"border-left":e,"border-left-width":e,"border-radius":e,"border-right":e,"border-right-width":e,"border-top":e,"border-top-left-radius":e,"border-top-right-radius":e,"border-top-width":e,"border-width":e,margin:e,"margin-bottom":e,"margin-left":e,"margin-right":e,"margin-top":e,padding:e,"padding-bottom":e,"padding-left":e,"padding-right":e,"padding-top":e,"mask-position-x":e,"mask-position-y":e,"mask-size":e,height:e,width:e,"min-height":e,"max-height":e,"min-width":e,"max-width":e,bottom:e,left:e,top:e,right:e,"box-shadow":e,"text-shadow":e,"column-gap":e,"column-rule":e,"column-rule-width":e,"column-width":e,"font-size":e,"font-size-delta":e,"letter-spacing":e,"text-indent":e,"text-stroke":e,"text-stroke-width":e,"word-spacing":e,motion:e,"motion-offset":e,outline:e,"outline-offset":e,"outline-width":e,perspective:e,"perspective-origin-x":a,"perspective-origin-y":a,"transform-origin":a,"transform-origin-x":a,"transform-origin-y":a,"transform-origin-z":a,"transition-delay":n,"transition-duration":n,"vertical-align":e,"flex-basis":e,"shape-margin":e,size:e,grid:e,"grid-gap":e,"grid-row-gap":e,"grid-column-gap":e,"grid-template-rows":e,"grid-template-columns":e,"grid-auto-rows":e,"grid-auto-columns":e,"box-shadow-x":e,"box-shadow-y":e,"box-shadow-blur":e,"box-shadow-spread":e,"font-line-height":e,"text-shadow-x":e,"text-shadow-y":e,"text-shadow-blur":e});function p(t,o,r){if(!o)return o;if(Array.isArray(o))for(var i=0;i<o.length;i++)o[i]=p(t,o[i],r);else if("object"==typeof o)if("fallbacks"===t)for(var e in o)o[e]=p(e,o[e],r);else for(var n in o)o[n]=p(t+"-"+n,o[n],r);else if("number"==typeof o)return r[t]?""+o+r[t]:s[t]?"function"==typeof s[t]?s[t](o).toString():""+o+s[t]:o.toString();return o}o.a=function(t){void 0===t&&(t={});var o=d(t);return{onProcessStyle:function(t,r){if("style"!==r.type)return t;for(var i in t)t[i]=p(i,t[i],o);return t},onChangeValue:function(t,r){return p(r,t,o)}}}}}]);