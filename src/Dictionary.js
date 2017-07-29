var XLSX = require('xlsx-style-browserify');

exports.readDictionaryImpl =
function (filename, cb)
{
  return function(){

  /* set up XMLHttpRequest */
  var url = filename; //"test_files/formula_stress_test_ajax.xlsx";
  var oReq = new XMLHttpRequest();
  oReq.open("GET", url, true);
  oReq.responseType = "arraybuffer";

  oReq.onload = function(e) {
    var arraybuffer = oReq.response;

    /* convert data to binary string */
    var data = new Uint8Array(arraybuffer);
    var arr = new Array();
    for(var i = 0; i != data.length; ++i) arr[i] = String.fromCharCode(data[i]);
    var bstr = arr.join("");

    /* Call XLSX */
    var workbook = XLSX.read(bstr, {type:"binary"});

    /* DO SOMETHING WITH workbook HERE */
    //console.log((XLSX.utils.sheet_to_json(workbook.Sheets[workbook.SheetNames[0]])));
    //cb({XLSX.utils.sheet_to_json(workbook.Sheets[workbook.SheetNames[0]]))();
    cb((XLSX.utils.sheet_to_json(workbook.Sheets["词缀"]).concat((XLSX.utils.sheet_to_json(workbook.Sheets["词表"])))))();
  }

  oReq.send();

  console.log("LOAD!OK!");
}
}
