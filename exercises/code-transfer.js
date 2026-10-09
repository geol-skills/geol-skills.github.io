// Export every code box on an exercise page to one runnable .R file, and
// import such a file back into the boxes.
//
// Also keeps each box's code in the browser as the student types, so a
// reload brings it back. (quarto-live's own `persist` saves only on Run,
// and keys on block number and full URL: adding a cell, or arriving via a
// #anchor, loses the code.) Saved code is keyed on page path and label.
//
// Each box becomes an RStudio section: "# <heading> [<label>] ----".
// <label> is the cell's exercise label (w2_1), or for unlabelled boxes the
// id of the enclosing section. Import matches on the label only.
//
// Relies on two quarto-live internals; re-test if the extension is upgraded:
// - each editor's container has id "webr-N", and its cell options are in
//   <script type="webr-N-contents"> (base64 JSON);
// - CodeMirror's view hangs off .cm-content as cmView.view.
(function () {
  "use strict";

  // "# anything [label] ----". The dashes may be dropped for a label that is
  // on this page, so a student who trims them doesn't lose the marker.
  var MARKER = /^\s*#.*\[([A-Za-z0-9_.-]+)\]\s*(-*)\s*$/;

  function EditorView(content) {
    var cm = content.cmView;
    return cm && (cm.view || (cm.rootView && cm.rootView.view));
  }

  function CellData(id) {
    var script = document.querySelector('script[type="' + id + '-contents"]');
    if (!script) return {};
    try {
      var bytes = Uint8Array.from(atob(script.textContent), function (c) {
        return c.charCodeAt(0);
      });
      return JSON.parse(new TextDecoder().decode(bytes));
    } catch (e) {
      return {};
    }
  }

  function SectionOf(el) {
    var section = el.closest("section[id]");
    var heading = section && section.querySelector("h1, h2, h3, h4");
    return {
      id: section ? section.id : "box",
      heading: heading ? heading.textContent.replace(/\s+/g, " ").trim() : ""
    };
  }

  // Every editable box on the page, in page order.
  function Boxes() {
    var used = {};
    return Array.prototype.map.call(
      document.querySelectorAll(".exercise-editor .cm-content"),
      function (content, i) {
        var container = content.closest("[id^='webr-']");
        var id = container ? container.id : "";
        var cell = CellData(id);
        var attr = cell.attr || {};
        var section = SectionOf(content);
        var label = attr.exercise || section.id;
        if (used[label]) {
          used[label] += 1;
          label = label + "-" + used[label];
        } else {
          used[label] = 1;
        }
        return {
          n: i + 1,
          label: label,
          heading: section.heading || label,
          starter: typeof cell.code === "string" ? cell.code : null,
          view: EditorView(content),
          storageKey: "code-transfer:" + window.location.pathname + ":" + label
        };
      }
    );
  }

  function PageTitle() {
    var h = document.querySelector("h1.title");
    return h ? h.textContent.trim() : document.title;
  }

  function FileName() {
    var page = window.location.pathname.split("/").pop().replace(/\.html$/, "");
    var week = page.match(/^(week\d+)-app$/);
    return (week ? week[1] : page || "exercises") + ".R";
  }

  function Today() {
    return new Date().toISOString().slice(0, 10);
  }

  function Report(where, text, ok) {
    var out = document.querySelectorAll(".code-transfer-report");
    out.forEach(function (el) {
      if (where && !where.contains(el)) return;
      el.textContent = text;
      el.classList.toggle("text-danger", !ok);
    });
  }

  function Export(button) {
    var boxes = Boxes();
    if (!boxes.length) {
      Report(button.closest(".code-transfer"),
             "The code boxes haven't loaded yet. Wait a moment and try again.",
             false);
      return;
    }
    var unreadable = [];
    var lines = [
      "# " + PageTitle(),
      "# Exported from " + window.location.href.split("#")[0] + " on " + Today() + ".",
      "#",
      "# Each line ending in [label] ---- marks one code box on that page.",
      "# Keep those lines if you want to use 'Import code' on this file later;",
      "# you can change the words before [label].",
      "# Unfinished blanks (______) stop this file from running until you fill them in.",
      ""
    ];
    boxes.forEach(function (box) {
      var code = box.view ? box.view.state.doc.toString() : null;
      if (code === null) {
        unreadable.push(box.n);
        code = "# (Couldn't read this box.)";
      }
      lines.push("# " + box.heading + " [" + box.label + "] ----");
      lines.push(code);
      lines.push("");
    });
    var blob = new Blob([lines.join("\n")], { type: "text/plain" });
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = FileName();
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(function () { URL.revokeObjectURL(a.href); }, 1000);
    Report(button.closest(".code-transfer"),
           unreadable.length ?
             "Exported, but couldn't read box " + unreadable.join(", ") +
               ". Copy those by hand, and please tell us on the board." :
             "Saved " + boxes.length + " boxes to " + FileName() + ".",
           !unreadable.length);
  }

  // Split a file into { label: code } in file order, plus anything that
  // came before the first marker.
  function Parse(text, known) {
    var lines = text.replace(/\r\n?/g, "\n").split("\n");
    var sections = [];
    var preamble = [];
    var current = null;
    lines.forEach(function (line) {
      var m = line.match(MARKER);
      if (m && (m[2].length >= 2 || (known && known[m[1]]))) {
        current = { label: m[1], lines: [] };
        sections.push(current);
      } else if (current) {
        current.lines.push(line);
      } else {
        preamble.push(line);
      }
    });
    sections.forEach(function (s) {
      // Export adds one blank line after each box; take it back off.
      if (s.lines.length && s.lines[s.lines.length - 1] === "") s.lines.pop();
      s.code = s.lines.join("\n");
    });
    var header = /^\s*(#.*)?$/;
    return {
      sections: sections,
      strayCode: preamble.some(function (l) { return !header.test(l); })
    };
  }

  function Save(box) {
    if (!box.view) return;
    try {
      window.localStorage.setItem(box.storageKey, box.view.state.doc.toString());
    } catch (e) {}
  }

  function Saved(box) {
    try {
      return window.localStorage.getItem(box.storageKey);
    } catch (e) {
      return null;
    }
  }

  function SaveAll() {
    Boxes().forEach(Save);
  }

  // Put saved code back once the editors exist. Only boxes still showing
  // their starter code are touched, so nothing typed since load is lost.
  function Restore() {
    Boxes().forEach(function (box) {
      var saved = Saved(box);
      if (saved === null || !box.view) return;
      var now = box.view.state.doc.toString();
      if (now === box.starter && saved !== now) {
        box.view.dispatch({ changes: { from: 0, to: now.length, insert: saved } });
      }
    });
  }

  function WhenEditorsReady(callback) {
    var last = -1;
    var tries = 0;
    var timer = setInterval(function () {
      var n = document.querySelectorAll(".exercise-editor .cm-content").length;
      tries += 1;
      if ((n > 0 && n === last) || tries > 240) {
        clearInterval(timer);
        callback();
      }
      last = n;
    }, 250);
  }

  function SetCode(box, code) {
    var view = box.view;
    view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: code } });
    try {
      window.localStorage.setItem(box.storageKey, code);
    } catch (e) {}
  }

  function Import(file, where) {
    var reader = new FileReader();
    reader.onload = function () {
      var boxes = Boxes();
      if (!boxes.length) {
        Report(where, "The code boxes haven't loaded yet. Wait a moment and try again.", false);
        return;
      }
      var byLabel = {};
      boxes.forEach(function (b) { byLabel[b.label] = b; });
      var parsed = Parse(String(reader.result), byLabel);
      if (!parsed.sections.length) {
        Report(where, "This file has no [label] ---- lines, so there's no way to tell " +
               "which code goes in which box. Import only works on files made by " +
               "'Export all code'.", false);
        return;
      }
      var incoming = {};
      var unknown = [];
      parsed.sections.forEach(function (s) {
        if (!byLabel[s.label]) {
          unknown.push(s.label);
        } else if (s.label in incoming) {
          incoming[s.label] += "\n" + s.code;
        } else {
          incoming[s.label] = s.code;
        }
      });
      var targets = boxes.filter(function (b) { return b.label in incoming; });
      if (!targets.length) {
        Report(where, "None of the boxes in this file are on this page. " +
               "Is it from a different week?", false);
        return;
      }
      var broken = targets.filter(function (b) { return !b.view; });
      if (broken.length) {
        Report(where, "Couldn't reach box " +
               broken.map(function (b) { return b.n; }).join(", ") +
               ", so nothing was imported. Please tell us on the board.", false);
        return;
      }
      var clobbered = targets.filter(function (b) {
        var now = b.view.state.doc.toString();
        return now !== incoming[b.label] && b.starter !== null && now !== b.starter;
      });
      if (clobbered.length &&
          !window.confirm("This will replace the code you've written in " +
                          clobbered.length + (clobbered.length === 1 ? " box" : " boxes") +
                          " on this page. Continue?")) {
        Report(where, "Import cancelled; nothing changed.", true);
        return;
      }
      targets.forEach(function (b) { SetCode(b, incoming[b.label]); });

      var notes = ["Restored " + targets.length + " of " + boxes.length + " boxes."];
      var missing = boxes.filter(function (b) { return !(b.label in incoming); });
      if (missing.length) {
        notes.push("Not in the file: " +
                   missing.map(function (b) { return "[" + b.label + "]"; }).join(", ") +
                   " (left as they were).");
      }
      if (unknown.length) {
        notes.push("Not on this page, so skipped: " +
                   unknown.map(function (l) { return "[" + l + "]"; }).join(", ") + ".");
      }
      if (parsed.strayCode) {
        notes.push("Code above the first [label] line was skipped.");
      }
      notes.push("Run the boxes from the top to get the results back.");
      Report(where, notes.join(" "), !unknown.length && !parsed.strayCode);
    };
    reader.readAsText(file);
  }

  document.addEventListener("click", function (e) {
    var exp = e.target.closest(".code-transfer-export");
    if (exp) {
      e.preventDefault();
      Export(exp);
      return;
    }
    var imp = e.target.closest(".code-transfer-import");
    if (imp) {
      e.preventDefault();
      var input = document.createElement("input");
      input.type = "file";
      input.accept = ".R,.r,text/plain";
      input.addEventListener("change", function () {
        if (input.files.length) Import(input.files[0], imp.closest(".code-transfer"));
      });
      input.click();
    }
  });

  var pending = null;
  function SaveSoon() {
    clearTimeout(pending);
    pending = setTimeout(SaveAll, 300);
  }
  var ready = false;
  WhenEditorsReady(function () {
    Restore();
    ready = true;
  });
  ["input", "keyup", "paste", "cut", "drop", "click"].forEach(function (type) {
    document.addEventListener(type, function (e) {
      if (ready && e.target.closest && e.target.closest(".exercise-editor")) SaveSoon();
    }, true);
  });
  window.addEventListener("pagehide", function () {
    if (ready) SaveAll();
  });

  window.CodeTransfer = { Boxes: Boxes, SaveAll: SaveAll, Parse: Parse, Export: Export, Import: Import };
})();
