function json2messages (json)
{
  var rows = json.rows.map (
    function (row) {
      var owner = row.key[0],
          trace_id = row.key[1],
          id = row.id,
          prog_id = row.value.prog_id,
          host = row.value.host,
          message = row.value.message;
      var datetime =
          dateFormat(new Date(row.value.timestamp),
                     'mmm dd, yyyy HH:MM:ss.L');
      return "<div class='entry toload' id='"+id+"' style='margin-left: 15px'>"
               +"<span class='key expandable log closed' id='"+id+"_key'>"
                 +"<span class='datetime'>"+datetime+"</span>"
                 +"<span class='separator1'> - </span>"
                 +"<span class='prog_id'>"+prog_id+" ("+host+")</span>"
                 +"<span class='separator1'> - </span>"
                 +"<span class='message'>"+message+"</span>"
               +"</span>"
               +"<div class='value' id='"+id+"_val' style='display: none;'>"
               +"</div>"
            +"</div>";
    });
  return rows.join("\n");
}

function json2divs (json)
{
  return json2divs1 (json._id, json, 1);
}

function escapeHtml (html)
{
  var escaped = html;
  if (typeof escaped == "string")
    {
      escaped = escaped.replace(/&/g, "&amp;");
      escaped = escaped.replace(/</g, "&lt;");
      escaped = escaped.replace(/</g, "&lt;");
      escaped = escaped.replace(/"/g, "&quot;");
    }
  return escaped;
}

function normalizeKey (key)
{
  return key.replace(/\./g,"").replace(/\s/g,"").replace(/\"/g,"").replace(/\{/g,"").replace(/\}/g,"").replace(/\(/g,"").replace(/\)/g,"").replace(/:/g,"").replace(/\[/g,"").replace(/\]/g,"").replace(/,/g,"").replace(/\?/g,"");
}

function json2divs1 (id, json, level)
{
  var tmp = "",
      a = [],
      k,
      key;

  for (key in json)
    {
      if (json.hasOwnProperty(key))
        {
          a.push(key);
        }
    }
  a.sort ();

  for (k = 0; k < a.length; k++)
    {
      key = a[k];
      if (   key != "_id"        // ignore couch fields
          && key != "_rev"
          && key != "_revisions"
          && key.indexOf("mondemand.") != 0 // ignore mondemand fields 
          && key != "SenderIP"  // ignore lwes fields 
          && key != "SenderPort"
          && key != "EventName"
          && key != "ReceiptTime"
          && key != "enc" )
        {
          var did =
            id+"_"+level+"_"+normalizeKey(key)+"_"
            +(Math.floor(Math.random()*1000000));
          tmp += "<div class='entry'"
                 + " style='margin-left: "+15+"px;'"
                 + " id='"+did+"'"
                 + ">";
          if (typeof json[key] === "object")
            {
              tmp +=
                 "<span class='key expandable col1 closed' id='"+did+"_key' >"
               + key
               + "</span>"
               + "<span class='break'>&nbsp;</span>"
               + "<div class='value' id='"+did+"_val' style='display: none;'>"
               +   json2divs1 (id, json[key], level+1)
               + "</div>";
            }
          else
            {
              tmp +=
                "<span class='key col1 none' id='"+did+"_key' style='width:"+(400-level*15)+"px;'>"
              + key
              + "</span>"
              + "<span class='separator2'> : </span>"
              + "<span class='value' id='"+did+"_val'>"
              + escapeHtml (json[key])
              + "</span>"
              + "<span class='break'>&nbsp;</span>";
            }
          tmp += "</div>";
        }
    }

  return tmp;
}


