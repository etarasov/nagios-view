
  var detailsPopup;
  var detailsHost, detailsService, detailsState, detailsDescr, detailsStart, detailsEnd, detailsDur;
  function init() {
     detailsPopup = document.getElementById("detailsPopup");
     detailsHost = document.getElementById("host");
     detailsService = document.getElementById("service");
     detailsState = document.getElementById("state");
     detailsDescr = document.getElementById("descr");
     detailsStart = document.getElementById("start");
     detailsEnd = document.getElementById("end");
     detailsDur = document.getElementById("dur");
  }
  function f(info) { }
  function s(evt, host, service, state, descr, start, end, dur) {
    detailsPopup.style.left = evt.clientX + 10 + "px";
    detailsPopup.style.visibility = "hidden";
    detailsPopup.style.display = "block";
    var top = evt.clientY + 20 + "px";
    detailsPopup.style.top = evt.clientY + 20;
    detailsHost.innerHTML = host;
    detailsService.innerHTML = service;
    detailsState.innerHTML = state;
    detailsDescr.innerHTML = descr;
    detailsStart.innerHTML = start;
    detailsEnd.innerHTML = end;
    detailsDur.innerHTML = dur;
    if (top + detailsPopup.offsetHeight > document.body.clientHeight)
        top = evt.clientY - detailsPopup.offsetHeight - 20 + "px";
    detailsPopup.style.top = top;
    detailsPopup.style.visibility = "visible";
  }
  function c() { detailsPopup.style.display = "none"; }
