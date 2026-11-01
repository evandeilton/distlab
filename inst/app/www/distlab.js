/* distlab client script: KaTeX rendering, live language switch and
   distribution buttons. */
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

  // New content (renderUI, insertUI, tabs) is rendered as soon as it lands.
  var observer = new MutationObserver(function (mutations) {
    mutations.forEach(function (m) {
      m.addedNodes.forEach(function (n) {
        if (n.nodeType === 1) renderMath(n);
      });
    });
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
    // Distribution buttons (Home chips, related card) send their id.
    var b = ev.target.closest(".dl-goto");
    if (b && window.Shiny) {
      window.Shiny.setInputValue(b.getAttribute("data-input"),
        b.getAttribute("data-dist"), { priority: "event" });
    }
  });

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

  if (window.Shiny) {
    window.Shiny.addCustomMessageHandler("dl-lang", function (msg) {
      applyLanguage(msg.lang, msg.dict);
    });
  } else {
    document.addEventListener("shiny:connected", function () {
      window.Shiny.addCustomMessageHandler("dl-lang", function (msg) {
        applyLanguage(msg.lang, msg.dict);
      });
    });
  }
})();
