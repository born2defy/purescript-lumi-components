(window.webpackJsonp=window.webpackJsonp||[]).push([[7],{"+tVf":function(e,t,n){"use strict";var a=n("s0Qv"),o=n("ll6l"),l=n("H2/w"),i=n("SXP8"),s=n("bP3h"),r=n("7HZk"),u=n("sfHK"),c=r.createComponent("TabsLayout"),d=r.makeStateless(c)(function(e){return s.tabs({style:u.css({paddingLeft:"24px"}),tabStyle:u.css({}),selectedTabStyle:u.css({}),currentLocation:e.currentLocation,queryKey:e.queryKey,useHash:e.useHash,navigate:e.navigate,tabs:o.mapFlipped(o.functorArray)(i.oneOf(a.alternativeArray)(e.tabs))(function(e){return{content:e.content,id:e.id,label:e.label,count:e.count,testId:l.Nothing.value}})})});e.exports={component:c,tabLayout:d}},HDqs:function(e,t,n){"use strict";var a,o,l,i,s,r,u,c=n("YKFa"),d=n("Z/IS"),m=n("6nUn"),f=n("XSw5"),p=n("H2/w"),h=n("5Iaq"),g=n("SXP8"),y=n("U4xy"),b=n("RqP8"),w=n("aa8G"),v=n("Qku0"),C=n("+O+U"),L=n("5JIH"),k=n("SZBU"),N=n("6g2T"),x=n("5L+5"),S=n("0nsp"),q=n("i6IA"),I=n("hcG8"),H=n("+tVf"),_=n("TKTp"),P=n("8yhI"),z=n("qz/O"),R=n("PsKH"),A=n("bP3h"),J=n("qSYj"),T=n("pMgY"),D=n("xhm4"),U=n("7HZk"),K=n("sfHK"),O=n("h+YZ"),j=n("LQUV"),M=U.createComponent("LayoutExample"),F=(l=J.table({name:"Items",dropdownMenu:!0,sortable:!0,sort:y.toNullable(p.Nothing.value),sortBy:y.toNullable(p.Nothing.value),updateSort:function(e,t){return C.log("update sort click")()},selectable:!0,selected:y.toNullable(p.Nothing.value),onSelect:L.mkEffectFn1((a=b.show(b.showArray(b.showString)),function(e){return C.log(a(e))})),rows:[{rowLink:"/#/color",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg",title:"Gummed Paper Tape",link:"/#/color",dimensions:'10" x 12" x 13"',createdDate:"2018-02-12",id:"123"},{rowLink:"/#/text",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg",title:"Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape",link:"/#/text",dimensions:'2" Lightweight Poly',createdDate:"2018-02-12",id:"456"},{rowLink:"/#/input",imgSrc:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/14fdd720a7dc2373cc833eb8dd471784.jpg",title:"Poly Mailers",link:"/#/input",dimensions:'7.50" x 10.50"',createdDate:"2018-02-12",id:"789"}],getRowKey:function(e){return e.id},rowEq:m.eq(m.eqRec()(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowCons(m.eqRowNil)()(new v.IsSymbol(function(){return"title"}))(m.eqString))()(new v.IsSymbol(function(){return"rowLink"}))(m.eqString))()(new v.IsSymbol(function(){return"link"}))(j.eqURL))()(new v.IsSymbol(function(){return"imgSrc"}))(m.eqString))()(new v.IsSymbol(function(){return"id"}))(m.eqString))()(new v.IsSymbol(function(){return"dimensions"}))(m.eqString))()(new v.IsSymbol(function(){return"createdDate"}))(m.eqString))),onNavigate:function(e){return C.log("Should navigate to: "+h.un(j.newtypeURL)(j.URL)(e))()},variant:y.toNullable(p.Nothing.value),primaryColumn:y.toNullable(new p.Just({name:"product",label:y.toNullable(new p.Just("Product title")),filterLabel:y.toNullable(new p.Just("Product title")),sortBy:y.toNullable(p.Nothing.value),style:K.css({}),getLink:y.notNull(function(e){return e.link}),sticky:!1,renderCell:function(e){return z.lockup({image:p.Just.create(S.productThumb_({size:R.Small.value,image:O.img()({src:e.imgSrc})})),title:K.text(e.title),subtitle:p.Nothing.value})}})),columns:[{required:!0,name:"product-type",label:y.toNullable(new p.Just("Product type")),filterLabel:y.toNullable(p.Nothing.value),sortBy:y.toNullable(p.Just.create("title")),style:K.css({}),hidden:!1,sticky:!1,renderCell:function(e){return _.link({className:_.defaults.className,href:e.link,navigate:_.defaults.navigate,style:_.defaults.style,target:_.defaults.target,testId:_.defaults.testId,text:K.text(e.title)})}},{required:!0,name:"created-date",label:y.toNullable(new p.Just("Created date")),filterLabel:y.toNullable(p.Nothing.value),sortBy:y.toNullable(p.Just.create("createdDate")),style:K.css({}),hidden:!1,sticky:!1,renderCell:function(e){return K.text(e.createdDate)}}],onColumnChange:y.toNullable(p.Nothing.value)}),i=[[T.body_("User"),z.lockup({title:K.text("Flexo R."),subtitle:p.Just.create(K.text("Lumi")),image:p.Just.create(S.avatar_({size:R.Large.value,image:O.img()({src:"https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"})}))})],[T.body_("ID"),_.link({className:_.defaults.className,href:"/",navigate:_.defaults.navigate,style:K.css({}),target:_.defaults.target,testId:_.defaults.testId,text:T.body_("12345")})],[T.body_("Created"),T.body_("2018-09-02")]],s=q.layout(O.div()({children:[T.sectionHeader_("Items"),P.list({size:P.defaultList.size,rightAligned:P.defaultList.rightAligned,rows:i}),T.sectionHeader_("BarFoo"),P.list({size:P.defaultList.size,rightAligned:P.defaultList.rightAligned,rows:i})]})),r=[[T.body_("ID"),_.link({className:_.defaults.className,href:"/1234",navigate:_.defaults.navigate,style:K.css({}),target:_.defaults.target,testId:_.defaults.testId,text:T.body_("1234")})],[T.body_("Created"),T.body_("2018-09-02")]],u=q.layout(O.div()({children:[T.sectionHeader_("Details"),P.list({size:P.defaultList.size,rightAligned:P.defaultList.rightAligned,rows:r}),T.sectionHeader_("Foobar"),P.list({size:P.defaultList.size,rightAligned:P.defaultList.rightAligned,rows:r})]})),o=D.withRouter(U.toReactComponent()(c.identity(c.categoryFn))(M)({render:function(e){return t=e.props,N.column_([T.h2_("Centered layout"),x.example(q.layout(O.div()({children:[T.h2_("lorem ipsum")]}))),T.h2_("Tabs layout"),x.example(H.tabLayout({currentLocation:j.URL("#"+(t.location.pathname+(t.location.search+t.location.hash))),useHash:!0,navigate:new p.Just(function(e){var n=A.urlParts(e),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=p.fromMaybe("")(f.flip(d.index)(1)(w.split("#")(a)));return function(){return t.history.push(j.URL(o))}}),queryKey:"layout-demo-1",tabs:new g.NonEmpty({id:"details",label:"Details",count:p.Nothing.value,content:function(e){return O.div()({children:[T.p_("lorem ipsum")],style:K.css({padding:"5px"})})}},[{id:"shipment",label:"Shipment",count:new p.Just(3),content:function(e){return O.div()({children:[T.p_("dolor sit amet")],style:K.css({padding:"5px"})})}}])})),T.h2_("Centered layout"),x.example(q.layout(O.div()({children:[T.sectionHeader_("Details"),P.list({size:P.compactList.size,rightAligned:P.compactList.rightAligned,rows:r}),T.sectionHeader_("Foobar"),P.list({size:P.compactList.size,rightAligned:P.compactList.rightAligned,rows:r})]}))),T.h2_("One Column w/ Header layout"),x.example(I.layout({titleContent:K.text("Title"),additionalHeaderContent:T.p_("subheader"),actionContent:k.button({accessibilityLabel:k.defaults.accessibilityLabel,color:k.defaults.color,onPress:k.defaults.onPress,size:k.defaults.size,style:k.defaults.style,testId:k.defaults.testId,title:"Action",type:k.defaults.type,buttonState:k.defaults.buttonState}),mainContent:l,sidebarContent:p.Nothing.value})),T.h2_("One Column w/ Header layout"),x.example(I.layout({titleContent:K.text("Title"),additionalHeaderContent:T.p_("subheader"),actionContent:k.button({accessibilityLabel:k.defaults.accessibilityLabel,color:k.defaults.color,onPress:k.defaults.onPress,size:k.defaults.size,style:k.defaults.style,testId:k.defaults.testId,title:"Action",type:k.defaults.type,buttonState:k.defaults.buttonState}),mainContent:O.div()({children:[H.tabLayout({currentLocation:j.URL("#"+(t.location.pathname+(t.location.search+t.location.hash))),useHash:!0,navigate:new p.Just(function(e){var n=A.urlParts(e),a=n.path+(n.query+(n.hash.path+n.hash.query)),o=p.fromMaybe("")(f.flip(d.index)(1)(w.split("#")(a)));return function(){return t.history.push(j.URL(o))}}),queryKey:"layout-demo-2",tabs:new g.NonEmpty({id:"foo",label:"Items",count:new p.Just(2),content:function(e){return s}},[{id:"bar",label:"Details",count:p.Nothing.value,content:function(e){return u}}])})],style:K.css({})}),sidebarContent:p.Nothing.value}))]);var t}})),U.element(o)({}));e.exports={component:M,docs:F}},hcG8:function(e,t,n){"use strict";var a,o,l,i=n("H2/w"),s=n("pMgY"),r=n("7HZk"),u=n("sfHK"),c=n("h+YZ"),d=n("z85V"),m=r.createComponent("Sidebar"),f=r.makeStateless(m)(function(e){return r.fragment([c.div()({style:u.css({flex:"0 0 30%"}),className:"column bl view-scroll",maxLength:400,children:[c.div()({className:"ppa",children:[e.content]})]})])}),p=r.createComponent("OneColumnWithHeader"),h=(a=r.element(d.unsafeCreateDOMComponent("lumi-layout-view-head")),o=r.element(d.unsafeCreateDOMComponent("lumi-layout-view-body")),l=r.element(d.unsafeCreateDOMComponent("lumi-layout")),r.makeStateless(p)(function(e){return l({children:[a({children:[s.text({children:[e.titleContent],className:s.h2.className,color:s.h2.color,style:u.css({paddingBottom:"0",fontWeight:"normal",marginRight:"12px",flex:"none"}),tag:s.h2.tag,testId:s.h2.testId}),c.span()({style:u.css({overflow:"visible",flex:"1 1 70%",wordWrap:"break-word",padding:"0"}),children:[e.additionalHeaderContent]}),c.div()({style:u.css({flex:"1 1 10px"})}),c.div()({style:u.css({flex:"none"}),children:[e.actionContent]})]}),o({className:"view-body view-scroll",children:function(){if(e.sidebarContent instanceof i.Nothing)return[e.mainContent];if(e.sidebarContent instanceof i.Just)return[c.div()({className:"row row-justify-between row-no-padding",children:[c.div()({className:"column view-scroll",children:[e.mainContent]}),f({content:e.sidebarContent.value0})]})];throw new Error("Failed pattern match at Lumi.Components.Layouts.OneColumnWithHeader (line 59, column 29 - line 70, column 24): "+[e.sidebarContent.constructor.name])}()})]})}));e.exports={component:p,layout:h,sidebarLayoutComponent:m,sidebarLayout:f}},i6IA:function(e,t,n){"use strict";var a=n("7HZk"),o=n("z85V");e.exports={layout:function(e){return a.element(o.unsafeCreateDOMComponent("lumi-layout-centered"))({children:[e]})}}}}]);