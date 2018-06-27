Survey
    .StylesManager
    .applyTheme("darkblue");

var surveyJSON = $.getJSON(window.location.pathname + "/get", "", onDataLoad);

function onDataLoad(surveyJSON) {
  var survey = new Survey.Model(surveyJSON.aspects);
  survey.data = surveyJSON.answers

  //Create showdown mardown converter
  var converter = new showdown.Converter();
  survey.onTextMarkdown.add(function(survey, options){
      //convert the mardown text to html
      var str = converter.makeHtml(options.text.replace(/\\n/mg,"\n")).replace(/(<p[^>]+?>|<p>|<\/p>)/img, "");
      //remove root paragraphs <p></p>
      //str = str.substring(3);
      //str = str.substring(0, str.length - 4);
      //set html
      options.html = str;
  });


  $("#surveyContainer").Survey({
      model:survey,
      onComplete:sendDataToServer,
      onValueChanged:valueChanged
  });

  updateColors()
}

function sendDataToServer(survey) {
  $.ajax({
   url: window.location.pathname + "/post",
   type: 'POST',
   contentType:'application/json',
   data: JSON.stringify(survey.data),
   dataType:'json'
  });
}

colors = {
  "0":"#ffffb2",
  "1":"#fed976",
  "2":"#feb24c",
  "3":"#fd8d3c",
  "4":"#f03b20",
  "5":"#bd0026"
}

function valueChanged(sender, options) {
  console.log(options.name)
  console.log(options.value)

  $("input[name='" + options.name + "']").parent().parent().children("label").removeClass("active")
  $("input[name='" + options.name + "'][value='" + options.value + "']").parent().addClass("active")

  updateColors()

  setTimeout(updateColors, 1)
}


function updateColors() {
  //reset
  $("span.sv_q_rating_item_text").css({"background-color":"", "border":""})

  // apply color to active
  labels = $("label.active")
  values = labels.children("input")

  labels.map(function() {
    label = $(this)
    value = label.children("input").attr("value")
    label
      .children("span.sv_q_rating_item_text")
      .css({"background-color":colors[value], "border":"1px solid #000"})
  })
}
