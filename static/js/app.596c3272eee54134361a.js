webpackJsonp([1],{163:function(t,n,e){function i(t){e(52)}var a=e(41)(e(49),e(164),i,"data-v-01913a2e",null);t.exports=a.exports},164:function(t,n){t.exports={render:function(){var t=this,n=t.$createElement,e=t._self._c||n;return e("div",{staticClass:"hello-page"},[e("el-row",{attrs:{gutter:10,type:"flex",justify:"center"}},[e("el-col",{attrs:{xs:24,sm:24,md:10,lg:10}},[e("h1",[t.loading?t._e():e("span",[t._v("欢迎使用Simplingua词典")]),t._v(" "),t.loading?e("span",[e("i",{staticClass:"el-icon-loading"}),t._v(" 词典读取中...")]):t._e()])])],1),t._v(" "),t.loading?t._e():e("el-row",{attrs:{gutter:10,type:"flex",justify:"center"}},[e("el-col",{attrs:{xs:24,sm:24,md:10,lg:10}},[e("el-input",{attrs:{placeholder:"请输入单词",icon:"search","on-icon-click":t.search},nativeOn:{keyup:function(n){if(!("button"in n)&&t._k(n.keyCode,"enter",13))return null;t.search(n)}},model:{value:t.keyword,callback:function(n){t.keyword=n},expression:"keyword"}})],1)],1),t._v(" "),e("div",{directives:[{name:"show",rawName:"v-show",value:t.entrys.length>0,expression:"entrys.length > 0"}],staticClass:"entrys"},t._l(t.entrys,function(n){return e("el-row",{key:n.simplingua,attrs:{gutter:10,type:"flex",justify:"center"}},[e("el-col",{attrs:{xs:24,sm:24,md:10,lg:10}},[e("div",{staticClass:"entry"},[e("div",{staticClass:"top"},[e("div",{staticClass:"simplingua"},[t._v("\n              "+t._s(t._f("capitalize")(n.simplingua))+"\n            ")]),t._v(" "),e("div",{staticClass:"type"},[e("el-tag",{staticClass:"type-tag"},[t._v(t._s(n.type))]),t._v(" "),e("el-tag",{staticClass:"rank-tag"},[t._v("Rank "+t._s(n.rank))])],1),t._v(" "),e("div",{staticClass:"clear"})]),t._v(" "),e("ul",{staticClass:"explain"},t._l(t.enter(n.explain),function(n){return e("li",[t._v("\n              "+t._s(n)+"\n            ")])})),t._v(" "),e("div",{staticClass:"root"},[t._v("\n            "+t._s(n.root)+"\n          ")])])])],1)})),t._v(" "),e("el-row",{staticClass:"footer",attrs:{gutter:10,type:"flex",justify:"center"}},[e("el-col",{attrs:{xs:24,sm:24,md:10,lg:10}},[e("a",{attrs:{href:"/static/textbook.doc"}},[t._v("课本书")]),t._v(" "),e("a",{attrs:{href:"/static/simplingua.xlsx"}},[t._v("词典")]),t._v(" "),e("a",{attrs:{href:"/static/grammar.doc"}},[t._v("语法书")]),t._v(" "),e("a",{attrs:{href:"https://github.com/bydmm/simplingua-workflow"}},[t._v("Alfred Workflow词典插件")])])],1)],1)},staticRenderFns:[]}},165:function(t,n){t.exports={render:function(){var t=this,n=t.$createElement,e=t._self._c||n;return e("div",{staticClass:"contianer"},[e("div",{staticClass:"bk",attrs:{id:"js-bk"}}),t._v(" "),e("div",{attrs:{id:"app"}},[e("router-view")],1)])},staticRenderFns:[]}},168:function(t,n){},169:function(t,n){},170:function(t,n){},171:function(t,n){},18:function(t,n){},43:function(t,n,e){"use strict";var i=e(17),a=e(166),s=e(163),r=e.n(s);i.default.use(a.a),n.a=new a.a({routes:[{path:"/",name:"Hello",component:r.a}]})},45:function(t,n,e){function i(t){e(53)}var a=e(41)(e(48),e(165),i,null,null);t.exports=a.exports},47:function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var i=e(18),a=(e.n(i),e(44)),s=e.n(a),r=e(17),o=e(45),c=e.n(o),l=e(43),u=e(46);r.default.config.productionTip=!1,r.default.use(s.a),r.default.use(u.a),new r.default({el:"#app",router:l.a,template:"<App/>",components:{App:c.a}})},48:function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var i=e(162),a=e.n(i);n.default={name:"app",mounted:function(){var t=a()({variance:.7,x_colors:["#15252d","#768766","#e5db82","#bb9944","#bb6622","#b1200f","#342e3a"],cell_size:140,width:window.innerWidth,height:window.innerHeight}).canvas();document.getElementById("js-bk").appendChild(t)}}},49:function(t,n,e){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var i=e(135),a=e.n(i),s=e(139),r=e.n(s);n.default={data:function(){return{keyword:"",word:"",dictionaries:[],loading:!0}},computed:{entrys:function(){var t=this.word;if(t.length<1)return[];var n=t.split(" ");return n.length>1?this.translationWords(n):this.translationWord(t)}},methods:{translationWords:function(t){return console.log(t),a()(this.dictionaries,function(n){return n.simplingua&&r()(t,n.simplingua)})},translationWord:function(t){var n=this.headMatch(t),e=this.bodyMatch(t),i=this.translationMatch(t);return n.concat(e).concat(i)},accurateMatch:function(t){this.dictionaries.forEach(function(n){if(n.simplingua&&n.simplingua===t)return[n]})},headMatch:function(t){return a()(this.dictionaries,function(n){return n.simplingua&&n.simplingua.match("^"+t+".*")})},bodyMatch:function(t){return a()(this.dictionaries,function(n){return n.explain&&n.explain.match(t+".*")})},translationMatch:function(t){return a()(this.dictionaries,function(n){return n.explain&&n.explain.match(t+".*")})},search:function(){console.log("search"),this.word=this.keyword.toLowerCase()},loadData:function(){var t=this;this.$http.get("/static/dictionaries.json").then(function(n){t.loading=!1,t.dictionaries=n.body.dictionaries},function(t){})},enter:function(t){return t?(t=t.toString(),t.split("\n")):[]}},created:function(){this.loadData()},filters:{capitalize:function(t){return t?(t=t.toString(),t.charAt(0).toUpperCase()+t.slice(1)):""}}}},52:function(t,n){},53:function(t,n){}},[47]);
//# sourceMappingURL=app.596c3272eee54134361a.js.map