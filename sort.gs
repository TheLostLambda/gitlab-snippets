var PEAKS = ["Capitol", "Evans", "Pikes", "Longs"];

function onOpen() {
  var menu = [{name: "Generate Schedules", functionName: "genSchedules"}, {name: "Dispatch Emails", functionName: "sendSchedules"}];
  SpreadsheetApp.getActive().addMenu("Miner Cup", menu);  
}

function genSchedules() {
  var ss = SpreadsheetApp.getActive();
  var sortedData = sortKiddos(ss);
  
  for(var i = 0; i < sortedData.length; i++) {
    var event = sortedData[i];
    var schedule = ss.getSheetByName(event.location);
    if(schedule != null) {
      schedule.clearContents();
    }
  }
  
  for(var i = 0; i < sortedData.length; i++) {
    var event = sortedData[i];
    var schedule = ss.getSheetByName(event.location);
    if(schedule == null) {
      schedule = ss.insertSheet();
      schedule.setName(event.location);
    }
    schedule.getRange(1, timeToColumn(event.startTime) - 5, 1, 1).setValue(event.startTime.split(" ")[0] + " - " + event.endTime.split(" ")[0] + ": " + event.name);
    if(event.players.length > 0) {
      schedule.getRange(2, timeToColumn(event.startTime) - 5, event.players.length, 1).setValues(columnise(getNames(event.players)));
    }
    schedule.autoResizeColumn(timeToColumn(event.startTime) - 5)
  }
  
  var sheets = events(ss);
  for(var i = 0; i < sheets.length; i++) {
    var schedule = ss.getSheetByName(sheets[i]);
    var range = schedule.getDataRange();
    var numRows = range.getNumRows();
    var numCols = range.getNumColumns();
    for (var r = 2; r <= numRows; r++) {
      for (var c = 1; c <= numCols; c++) {
        var cell = range.getCell(r,c)
        if(cell.getValue().indexOf("- Capitol") !== -1) {
          cell.setBackground("#5555FF");
        } else if(cell.getValue().indexOf("- Evans") !== -1) {
          cell.setBackground("#FF5555");
        } else if(cell.getValue().indexOf("- Pikes") !== -1) {
          cell.setBackground("#FF55FF");
        } else if(cell.getValue().indexOf("- Longs") !== -1) {
          cell.setBackground("#55FF55");
        } else {
          cell.setBackground("#FFFFFF")
        }
      }
    }
  }
}

function sendSchedules() {
  var ui = SpreadsheetApp.getUi();
  var response = ui.alert("Are You Sure?", "This will send an email to every student and is irreversible!", ui.ButtonSet.YES_NO);
  
  if(response == ui.Button.YES) {
    var ss = SpreadsheetApp.getActive();
    var responses = ss.getSheetByName("Form Responses").getDataRange().getValues();
    var sortedData = sortKiddos(ss);
    
    for(var i = 1; i < responses.length; i++) {
    var email = responses[i][12];
    var myEvents = sortedData.filter(containsStudent, email);
    myEvents = myEvents.sort(compareTimes);
    var message = "The time has come " + responses[i][3].trim() + "! Gear up for battle! It's Dangerous to go alone! Take this!\n\nYour Miner Cup Schedule:\n\n"
    for(var j = 0; j < myEvents.length; j++) {
      message += myEvents[j].startTime.split(" ")[0] + " - " + myEvents[j].endTime.split(" ")[0] + ": " + myEvents[j].name + "\n";
    }
    if(myEvents.length == 0) {
      message += "Empty? Hmm, out of room for your choices... Bug counseling!"
    }
    var subject = "Miner Cup Schedule";
    MailApp.sendEmail(email, subject, message);
    }
  }
}

function compareTimes(a,b) {
  var dateA = new Date("January 1, 1970 " + a.startTime);
  var dateB = new Date("January 1, 1970 " + b.startTime);
  if(dateA < dateB) {
    return -1;
  } else if (dateA > dateB) {
    return 1;
  } else {
    return 0;
  }
}

function columnise(arr) {
  return arr.map(function(v) { return [v] });
}

function getNames(arr) {
  return arr.map(function(v) { return v.name; });
}

function containsStudent(event) {
  // Actually FUCK Javascript... I need to set this to that or else the comparison below fails...
  // This cost me four hours of my life and it's 03:00 in the morning. Why in the hell does this work?
  var that = this;
  return event.players.filter(function(player) { return player.email == that }).length > 0;
}

function events(ss) {
  var eventData = ss.getSheetByName("Event Details");
  return eventData.getRange("C1:G1").getValues()[0];
}

function sortKiddos(ss) {
  var responses = ss.getSheetByName("Form Responses");
  var eventData = ss.getSheetByName("Event Details");
  var eventDetails = loadEvents(eventData);
  for(var i = 0; i < eventDetails.length; i++) {
    var event = eventDetails[i];
    var data = responses.getDataRange().getValues();
    timeSlot = timeToColumn(event.startTime);
    // Individual events
    if(event.teamSize == 1) {
      var interested = data.slice(1).filter(wantsEvent, event);
      for(var j = 0; j < interested.length; j++) {
        if(event.players.length < event.totalSize || event.totalSize == 0) {
          var player = interested[j]
            event.players.push({name: player[3].trim() + " " + player[2].trim() + " - " + player[4].trim(), email: player[12]});
        }
      }
    }
    // Team events
    else {
      var lastSize = event.players.length;
      for(var j = 0; j < PEAKS.length; j++) {
        var peak = PEAKS[j];
        var interested = data.slice(1).filter(inPeak, peak).filter(wantsEvent, event);
        for(var k = 0; k < interested.length; k++) {
          if(event.players.length - lastSize < event.teamSize) {
            var player = interested[k]
            event.players.push({name: player[3].trim() + " " + player[2].trim() + " - " + player[4].trim(), email: player[12]});
          }
        }
        lastSize += event.players.length - lastSize;
      }
    }
  }
  return eventDetails;
}

function wantsEvent(student) {
  return student[timeToColumn(this.startTime)] == this.name;
}

function inPeak(student) {
  return student[4] == this
}

function timeToColumn(time) {
  switch(time.split(" ")[0]) {
    case "9:50:00":
      return 6;
    case "10:25:00":
      return 7;
    case "11:00:00":
      return 8;
    case "11:35:00":
      return 9;
    case "12:40:00":
      return 10;
    case "1:15:00":
      return 11;
  }
}

function loadEvents(eventData) {
  var data = eventData.getDataRange().getValues();
  var events = []
  for(var c = 2; c < data[0].length; c++) {
    for(var r = 1; r < data.length; r++) {
      var e = data[r][c];
      var n = e.split("|")[0];
      var tS = e.split("|")[1];
      var s = e.split("|")[2];
      var l = data[0][c];
      var sT = data[r][0].toLocaleTimeString();
      var eT = data[r][1].toLocaleTimeString();
      if(e) {
        events.push({name: n.trim(), location: l, startTime: sT, endTime: eT, teamSize: tS.trim(), totalSize: s.trim(), players: []});
      }
    }
  }
  return events;
}