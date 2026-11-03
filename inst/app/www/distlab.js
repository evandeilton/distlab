/* distlab client script: KaTeX rendering, live language switch,
   distribution buttons, Home panels and last-distribution memory, copy
   buttons and the sortable, filterable tables. */
(function () {
  "use strict";

  // Render every pending .dl-math element below `root` with KaTeX.
  function renderMath(root) {
    if (!window.katex || !root || !root.querySelectorAll) return;
    var nodes = root.matches && root.matches(".dl-math") ? [root] : [];
    nodes = nodes.concat(Array.prototype.slice.call(
      root.querySelectorAll(".dl-math:not([data-rendered])")));
    nodes.forEach(function (el) {
      if (el.hasAttribute("data-rendered")) return;
      var tex = el.textContent;
      el.setAttribute("data-tex", tex);
      el.setAttribute("data-rendered", "true");
      try {
        window.katex.render(tex, el, {
          displayMode: el.getAttribute("data-display") === "true",
          throwOnError: false,
          strict: "ignore",
          output: "htmlAndMathml"
        });
      } catch (e) {
        el.textContent = tex;
      }
    });
  }

  // Lower case without accents, so "geometrica" finds "Geométrica".
  function fold(s) {
    return (s || "").normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase();
  }

  // Show only the rows of a table wrapper whose search text holds every word
  // typed in its filter box.
  function filterTable(wrap) {
    var box = document.querySelector('.dl-table-filter[data-target="' + wrap.id + '"]');
    var words = fold(box ? box.value : "").split(/\s+/).filter(Boolean);
    var shown = 0;
    wrap.querySelectorAll("tbody tr").forEach(function (tr) {
      var text = fold(tr.getAttribute("data-search"));
      var ok = words.every(function (w) { return text.indexOf(w) >= 0; });
      tr.hidden = !ok;
      if (ok) shown++;
    });
    var empty = wrap.querySelector(".dl-table-empty");
    if (empty) empty.hidden = shown > 0;
  }

  // Sort a table by column `col`; the state lives on the wrapper, which
  // survives re-rendering, so a new language keeps the order.
  function sortTable(wrap, col, dir) {
    var table = wrap.querySelector("table.dl-sortable");
    if (!table) return;
    var btn = table.querySelector('.dl-sort[data-col="' + col + '"]');
    if (!btn) return;
    var numeric = btn.getAttribute("data-numeric") === "true";
    var lang = document.documentElement.getAttribute("lang") || "en";
    var body = table.tBodies[0];
    var rows = Array.prototype.slice.call(body.rows);
    var key = function (tr) {
      var td = tr.cells[col];
      return td ? (td.getAttribute("data-sort") || td.textContent) : "";
    };
    rows.sort(function (a, b) {
      var r = numeric ? parseFloat(key(a)) - parseFloat(key(b)) :
        key(a).localeCompare(key(b), lang, { sensitivity: "base" });
      return dir === "descending" ? -r : r;
    });
    rows.forEach(function (tr) { body.appendChild(tr); });
    table.querySelectorAll("th[aria-sort]").forEach(function (th) {
      th.setAttribute("aria-sort", "none");
    });
    btn.closest("th").setAttribute("aria-sort", dir);
    wrap.setAttribute("data-sort-col", col);
    wrap.setAttribute("data-sort-dir", dir);
  }

  // Re-apply sort and filter after a table is (re)rendered.
  function refreshTables(root) {
    if (!root.querySelectorAll) return;
    var wraps = [];
    if (root.closest) {
      var w = root.closest(".dl-table-wrap");
      if (w) wraps.push(w);
    }
    root.querySelectorAll(".dl-table-wrap").forEach(function (w) { wraps.push(w); });
    wraps.forEach(function (w) {
      var col = w.getAttribute("data-sort-col");
      if (col !== null) sortTable(w, col, w.getAttribute("data-sort-dir") || "ascending");
      filterTable(w);
    });
  }

  // New content (renderUI, insertUI, tabs) is rendered as soon as it lands.
  var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.addedNodes.forEach(function (n) {
        if (n.nodeType !== 1) return;
        renderMath(n);
        if (n.querySelector && (n.matches(".dl-sortable") || n.querySelector(".dl-sortable"))) {
          refreshTables(n);
        }
      });
    });
  });

  document.addEventListener("input", function (ev) {
    var box = ev.target.closest && ev.target.closest(".dl-table-filter");
    if (!box) return;
    var wrap = document.getElementById(box.getAttribute("data-target"));
    if (wrap) filterTable(wrap);
  });

  // Fill {name} placeholders from data-i18n-<name> attributes of the element.
  function fill(text, el) {
    return text.replace(/\{([a-z_]+)\}/g, function (all, name) {
      var v = el.getAttribute("data-i18n-" + name);
      return v === null ? all : v;
    });
  }

  // Replace every translatable text and attribute with the new dictionary.
  function applyLanguage(lang, dict) {
    document.documentElement.setAttribute("lang", lang);
    document.querySelectorAll("[data-i18n]").forEach(function (el) {
      var t = dict[el.getAttribute("data-i18n")];
      if (typeof t === "string") el.textContent = fill(t, el);
    });
    ["title", "aria-label", "placeholder"].forEach(function (attr) {
      document.querySelectorAll("[data-i18n-" + attr + "]").forEach(function (el) {
        var t = dict[el.getAttribute("data-i18n-" + attr)];
        if (typeof t === "string") el.setAttribute(attr, t);
      });
    });
    document.querySelectorAll(".dl-dark-toggle").forEach(function (w) {
      ["dark", "light"].forEach(function (m) {
        var t = dict[w.getAttribute("data-i18n-label-" + m)];
        if (typeof t === "string") w.setAttribute("data-label-" + m, t);
      });
    });
    labelDarkToggle();
    setToggle(lang);
  }

  // The dark-mode switch renders an English label inside its shadow DOM;
  // copy the translated label for the next mode onto its button.
  function labelDarkToggle() {
    document.querySelectorAll(".dl-dark-toggle").forEach(function (w) {
      var el = w.querySelector("bslib-input-dark-mode");
      var btn = el && el.shadowRoot && el.shadowRoot.querySelector("button");
      if (!btn) return;
      var dark = document.documentElement.getAttribute("data-bs-theme") === "dark";
      var label = w.getAttribute(dark ? "data-label-light" : "data-label-dark");
      if (label) {
        btn.setAttribute("aria-label", label);
        btn.setAttribute("title", label);
      }
    });
  }

  function setToggle(lang) {
    document.querySelectorAll(".dl-lang-btn").forEach(function (b) {
      var on = b.getAttribute("data-lang") === lang;
      b.classList.toggle("active", on);
      b.setAttribute("aria-pressed", on ? "true" : "false");
    });
  }

  document.addEventListener("click", function (ev) {
    // Language buttons report the choice to the server.
    var lb = ev.target.closest(".dl-lang-btn");
    if (lb && window.Shiny) {
      var lang = lb.getAttribute("data-lang");
      setToggle(lang);
      window.Shiny.setInputValue("lang", lang);
      return;
    }
    // Home data-type tiles open their panel and close any other one.
    var gt = ev.target.closest(".dl-group-tile");
    if (gt) {
      toggleGroup(gt, gt.getAttribute("aria-expanded") !== "true");
      return;
    }
    // Distribution buttons (Home chips, related card, Compendium) send their id.
    var b = ev.target.closest(".dl-goto");
    if (b && window.Shiny) {
      window.Shiny.setInputValue(b.getAttribute("data-input"),
        b.getAttribute("data-dist"), { priority: "event" });
      return;
    }
    // Sortable table headers: ascending first, then toggle.
    var sb = ev.target.closest(".dl-sort");
    if (sb) {
      var wrap = sb.closest(".dl-table-wrap");
      var th = sb.closest("th");
      var dir = th.getAttribute("aria-sort") === "ascending" ? "descending" : "ascending";
      if (wrap) sortTable(wrap, sb.getAttribute("data-col"), dir);
      return;
    }
    // Copy buttons copy the text of their target element.
    var cb = ev.target.closest(".dl-copy");
    if (cb) {
      var src = document.getElementById(cb.getAttribute("data-target"));
      if (src) copyText(src.textContent, cb);
      return;
    }
    if (ev.target.closest(".dl-print")) window.print();
  });

  // Open or close the panel of a Home data-type tile; one is open at a time.
  function toggleGroup(tile, open) {
    var grid = tile.closest(".dl-group-grid");
    if (open && grid) {
      grid.querySelectorAll('.dl-group-tile[aria-expanded="true"]').forEach(function (t) {
        if (t !== tile) toggleGroup(t, false);
      });
    }
    tile.setAttribute("aria-expanded", open ? "true" : "false");
    var panel = document.getElementById(tile.getAttribute("aria-controls"));
    if (panel) panel.hidden = !open;
  }

  // Esc closes the open data-type panel and returns focus to its tile.
  document.addEventListener("keydown", function (ev) {
    if (ev.key !== "Escape" || !ev.target.closest) return;
    var grid = ev.target.closest(".dl-group-grid");
    var tile = grid && grid.querySelector('.dl-group-tile[aria-expanded="true"]');
    if (!tile) return;
    toggleGroup(tile, false);
    tile.focus();
  });

  // Last distribution opened in the Explorer or the Calculator, kept in the
  // browser for the Home resume strip. Storage may be blocked; then it is
  // simply not remembered.
  var lastKey = "distlab.last";
  function readLast() {
    try {
      var v = JSON.parse(window.localStorage.getItem(lastKey));
      return v && typeof v === "object" && typeof v.id === "string" ? v : null;
    } catch (e) {
      return null;
    }
  }
  function sendLast(v) {
    var home = document.querySelector("[data-last-input]");
    if (v && home && window.Shiny) {
      window.Shiny.setInputValue(home.getAttribute("data-last-input"), v);
    }
  }

  // Put the cursor in the Home search on wide screens with a mouse, where an
  // open keyboard does not cover the page.
  function focusSearch() {
    var home = document.querySelector(".dl-home");
    if (!home || !home.offsetParent) return;
    if (!window.matchMedia("(min-width: 992px) and (pointer: fine)").matches) return;
    var box = home.querySelector(".dl-home-search .selectize-input input");
    if (box) box.focus();
  }

  // Copy text to the clipboard and confirm on the button for a moment.
  function copyText(text, btn) {
    var label = btn.querySelector(".dl-copy-label");
    var done = function () {
      if (!label) return;
      var old = label.textContent;
      label.textContent = btn.getAttribute("data-done") || old;
      btn.classList.add("dl-copied");
      setTimeout(function () {
        label.textContent = old;
        btn.classList.remove("dl-copied");
      }, 1500);
    };
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(done, function () {});
      return;
    }
    // Fallback for pages served without a secure context.
    var area = document.createElement("textarea");
    area.value = text;
    area.setAttribute("readonly", "");
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.appendChild(area);
    area.select();
    try { if (document.execCommand("copy")) done(); } catch (e) { /* not available */ }
    document.body.removeChild(area);
  }

  document.addEventListener("DOMContentLoaded", function () {
    var t = document.getElementById("dl_lang");
    if (t) setToggle(t.getAttribute("data-initial"));
    renderMath(document.body);
    observer.observe(document.body, { childList: true, subtree: true });
    // Relabel after the switch renders and after every mode change.
    new MutationObserver(function () { setTimeout(labelDarkToggle, 0); })
      .observe(document.documentElement, { attributes: true, attributeFilter: ["data-bs-theme"] });
    setTimeout(labelDarkToggle, 0);
    if (window.customElements) {
      window.customElements.whenDefined("bslib-input-dark-mode").then(function () {
        setTimeout(labelDarkToggle, 50);
      });
    }
  });

  // Select a node or edge on the relationship map once the map is drawn
  // and visible (its tab may still be opening); retries for a few seconds.
  function mapSelect(msg, tries) {
    var el = document.getElementById("graph" + msg.map);
    var ready = el && el.chart && el.offsetParent !== null && el.clientWidth > 0;
    if (!ready) {
      if (tries > 0) setTimeout(function () { mapSelect(msg, tries - 1); }, 100);
      return;
    }
    var net = el.chart;
    var anim = { duration: 500, easingFunction: "easeInOutQuad" };
    if (msg.kind === "node") {
      net.selectNodes([msg.id]);
      net.focus(msg.id, { scale: 1.1, animation: anim });
    } else {
      net.selectEdges([msg.id]);
      net.fit({ nodes: msg.ends, maxZoomLevel: 1.1, animation: anim });
    }
  }

  function addHandlers() {
    window.Shiny.addCustomMessageHandler("dl-lang", function (msg) {
      applyLanguage(msg.lang, msg.dict);
    });
    window.Shiny.addCustomMessageHandler("dl-map-select", function (msg) {
      mapSelect(msg, 50);
    });
    // Show or hide elements by id (Compare slots and their buttons).
    window.Shiny.addCustomMessageHandler("dl-show", function (msg) {
      (msg.show || []).forEach(function (id) {
        var el = document.getElementById(id);
        if (el) el.hidden = false;
      });
      (msg.hide || []).forEach(function (id) {
        var el = document.getElementById(id);
        if (el) el.hidden = true;
      });
    });
    window.Shiny.addCustomMessageHandler("dl-remember", function (msg) {
      try {
        window.localStorage.setItem(lastKey, JSON.stringify(msg));
      } catch (e) { /* storage blocked or full */ }
      sendLast(msg);
    });
    // Mark the pressed button of a group (Compare presets).
    window.Shiny.addCustomMessageHandler("dl-pressed", function (msg) {
      document.querySelectorAll('.dl-goto[data-input="' + msg.group + '"]').forEach(function (b) {
        b.setAttribute("aria-pressed", b.getAttribute("data-dist") === msg.value ? "true" : "false");
      });
    });
  }

  if (window.Shiny) {
    addHandlers();
  } else {
    document.addEventListener("shiny:connected", addHandlers);
  }
  // Once connected, report the remembered distribution and focus the search.
  function onConnected() {
    sendLast(readLast());
    setTimeout(focusSearch, 100);
  }
  if (window.jQuery) {
    window.jQuery(document).one("shiny:connected", onConnected);
  } else {
    document.addEventListener("shiny:connected", onConnected, { once: true });
  }
})();
