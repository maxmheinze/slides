Reveal.on('slidechanged', event => {
  event.currentSlide.querySelectorAll("svg style").forEach(style => {
    style.remove();
  });

  event.currentSlide.querySelectorAll("svg text, svg tspan").forEach(el => {
    el.style.fontFamily = "Inter, sans-serif";
  });
});

// Also run on initial page load:
Reveal.on('ready', event => {
  document.querySelectorAll("svg style").forEach(style => {
    style.remove();
  });

  document.querySelectorAll("svg text, svg tspan").forEach(el => {
    el.style.fontFamily = "Inter, sans-serif";
  });
});