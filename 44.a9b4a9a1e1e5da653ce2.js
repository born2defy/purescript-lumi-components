(window.webpackJsonp=window.webpackJsonp||[]).push([[44],{967:function(e,t,n){"use strict";var r,o,l=n(5),i=n(11),s=n(20),a=n(220),u=n(137),c=n(45),d=n(29),m=n(8),f=n(103),g=n(1),y=n(22),p=n(14),b=n(21),w=n(9),S=n(43),k=n(16),q=n(3),h=n(304),R=n(48),N=n(176),C=n(321),v=n(72),I=n(303),x=n(257),L=n(129),B=n(430),D=n(113),P=n(439),E=n(40),F=n(24),M=n(26),T=n(76),j=n(102),J=F.createComponent("TableExample"),U=(r=[{rowLink:"http://google.com",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg",title:"Gummed Paper Tape",link:"/#/color",dimensions:'10" x 12" x 13"',createdDate:"2/12/2018",id:"f0pkl1"},{rowLink:"http://nytimes.com",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg",title:"Packing Tape",link:"/#/text",dimensions:'2" Lightweight Poly',createdDate:"2/01/2018",id:"10cms9"},{rowLink:"http://foobar.com",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg",title:"Packing Tape",link:"/#/text",dimensions:'2" Lightweight Poly',createdDate:"2/01/2018",id:"f2982"},{rowLink:"http://facebook.com",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/14fdd720a7dc2373cc833eb8dd471784.jpg",title:"Poly Mailers",link:"/#/input",dimensions:'7.50" x 10.50"',createdDate:"1/15/2018",id:"0mf7w"}],o={sort:"asc",sortBy:new g.Just("createdDate"),selected:["10cms9","0mf7w"]},F.make()(J)({initialState:o,render:function(e){return v.column_([E.p_("*Right click on table header to see FilterDropdownMenu"),I.example(v.column({style:M.css({alignSelf:"stretch",height:150,width:400}),children:[P.table({name:"Items",sortable:!0,sort:b.toNullable(new g.Just(e.state.sort)),sortBy:b.toNullable(e.state.sortBy),updateSort:function(t,n){return e.setState(function(e){return{sort:t,sortBy:b.toMaybe(n),selected:e.selected}})()},selectable:!0,selected:b.null,onSelect:R.mkEffectFn1(y.mempty(y.monoidFn(q.monoidEffect(y.monoidUnit)))),rows:(m.eq(P.eqSortString)(e.state.sort)("desc")?c.reverse:s.identity(s.categoryFn))((t=w.comparing(g.ordMaybe(w.ordString))(function(t){return i.bind(g.bindMaybe)(e.state.sortBy)(function(e){var n=a.runExcept(i.bindFlipped(u.bindExceptT(f.monadIdentity))(N.readString)(C.readProp(e)(N.unsafeToForeign(t))));return n instanceof d.Right?new g.Just(n.value0):g.Nothing.value})}),c.sortBy(t)(r))),getRowKey:function(e){return e.id},rowEq:m.eq(m.eqRec()(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowNil)()(new S.IsSymbol(function(){return"title"}))(m.eqString))()(new S.IsSymbol(function(){return"rowLink"}))(m.eqString))()(new S.IsSymbol(function(){return"link"}))(j.eqURL))()(new S.IsSymbol(function(){return"imgSrc"}))(m.eqString))()(new S.IsSymbol(function(){return"id"}))(m.eqString))()(new S.IsSymbol(function(){return"dimensions"}))(m.eqString))()(new S.IsSymbol(function(){return"createdDate"}))(m.eqString))),onNavigate:function(e){return h.log("navigate to: "+p.un(j.newtypeURL)(j.URL)(e))()},variant:b.null,primaryColumn:b.notNull({name:"product-lockup",label:b.null,filterLabel:b.notNull("Product lockup"),sortBy:b.null,style:M.css({}),getLink:function(e){return e.link},renderCell:function(e){return B.lockup({image:g.Just.create(x.productThumb_({image:T.img()({src:e.imgSrc}),size:D.Small.value})),title:M.text(e.title),subtitle:g.Just.create(M.text(e.dimensions))})},sticky:!0}),columns:[{required:!0,name:"product-type",label:b.notNull("Product type"),filterLabel:b.null,sortBy:b.notNull("title"),style:M.css({}),hidden:!1,sticky:!1,renderCell:function(e){return L.link({className:l.pure(g.applicativeMaybe)("action"),href:e.link,navigate:L.defaults.navigate,style:L.defaults.style,target:L.defaults.target,testId:L.defaults.testId,text:M.text(e.title)})}},{required:!0,name:"created-date",label:b.notNull("Created date"),filterLabel:b.null,sortBy:b.notNull("createdDate"),style:M.css({}),hidden:!1,sticky:!1,renderCell:function(e){return M.text(e.createdDate)}}]})]})),I.example(v.column({style:M.css({alignSelf:"stretch"}),children:[P.table({name:"Items",sortable:!0,sort:b.notNull(e.state.sort),sortBy:b.toNullable(e.state.sortBy),updateSort:function(t,n){return e.setState(function(e){return{sort:t,sortBy:b.toMaybe(n),selected:e.selected}})()},selectable:!0,selected:b.notNull(e.state.selected),onSelect:function(t){return e.setState(function(e){return{sort:e.sort,sortBy:e.sortBy,selected:t}})()},rows:(m.eq(P.eqSortString)(e.state.sort)("desc")?c.reverse:s.identity(s.categoryFn))(function(){var t=w.comparing(g.ordMaybe(w.ordString))(function(t){return i.bind(g.bindMaybe)(e.state.sortBy)(function(e){var n=a.runExcept(i.bindFlipped(u.bindExceptT(f.monadIdentity))(N.readString)(C.readProp(e)(N.unsafeToForeign(t))));return n instanceof d.Right?new g.Just(n.value0):g.Nothing.value})});return c.sortBy(t)(r)}()),getRowKey:function(e){return e.id},rowEq:m.eq(m.eqRec()(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowNil)()(new S.IsSymbol(function(){return"title"}))(m.eqString))()(new S.IsSymbol(function(){return"rowLink"}))(m.eqString))()(new S.IsSymbol(function(){return"link"}))(j.eqURL))()(new S.IsSymbol(function(){return"imgSrc"}))(m.eqString))()(new S.IsSymbol(function(){return"id"}))(m.eqString))()(new S.IsSymbol(function(){return"dimensions"}))(m.eqString))()(new S.IsSymbol(function(){return"createdDate"}))(m.eqString))),onNavigate:function(e){return h.log("navigate to: "+p.un(j.newtypeURL)(j.URL)(e))()},variant:b.notNull("compact"),primaryColumn:b.notNull({name:"product-title",label:b.notNull("Items"),filterLabel:b.notNull("Product title"),sortBy:b.null,style:M.css({}),getLink:function(e){return e.link},renderCell:function(e){return M.text(e.title)},sticky:!1}),columns:[{required:!0,name:"product-type",label:b.notNull("Product type"),filterLabel:b.null,sortBy:b.notNull("title"),style:M.css({}),hidden:!1,sticky:!1,renderCell:function(e){return L.link({className:L.defaults.className,href:e.link,navigate:L.defaults.navigate,style:L.defaults.style,target:L.defaults.target,testId:L.defaults.testId,text:M.text(e.title)})}},{required:!0,name:"created-date",label:b.notNull("Created date"),filterLabel:b.null,sortBy:b.notNull("createdDate"),style:M.css({}),hidden:!1,sticky:!1,renderCell:function(e){return M.text(e.createdDate)}}]})]}))]);var t}})(k.unit));e.exports={component:J,docs:U};n(0)}}]);