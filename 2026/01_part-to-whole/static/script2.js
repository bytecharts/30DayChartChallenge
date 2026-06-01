const chartEl = document.getElementById("chart-stage");
const tooltip = d3.select("#tooltip");
const playButton = document.getElementById("play-animation");

let animationTimer = null;

const parseColors = (text) => {
  const colorMap = new Map();
  text
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean)
    .forEach((line) => {
      const parts = line.split(":");
      if (parts.length < 2) return;
      const name = parts[0].trim();
      const color = parts.slice(1).join(":").trim();
      colorMap.set(name, color);
    });
  return colorMap;
};

const formatNumber = (value) =>
  new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 0,
  }).format(value);

const formatPercent = (value) => `${value.toFixed(1)}%`;

const buildBallSymbol = (svgText) => {
  const parser = new DOMParser();
  const doc = parser.parseFromString(svgText, "image/svg+xml");
  const svgEl = doc.querySelector("svg");
  if (!svgEl) return null;
  const paths = svgEl.querySelectorAll("path");
  if (paths.length > 0) {
    paths[0].setAttribute("fill", "currentColor");
  }
  return {
    viewBox: svgEl.getAttribute("viewBox") || "0 0 845 845",
    markup: svgEl.innerHTML,
  };
};

const draw = (data, colorMap, ballSymbol) => {
  const width = chartEl.clientWidth;
  const stageHeight = chartEl.clientHeight || 1080;
  const plotOffsetY = 12;
  const renderScale = 4; // try 2 or 3
  const plotHeight = Math.max(800, stageHeight - plotOffsetY - 140);

  d3.select(chartEl).select("svg").remove();

  const svg = d3
    .select(chartEl)
    .append("svg")
    .attr("width", width * renderScale)
    .attr("height", stageHeight * renderScale)
    .attr("viewBox", [0, 0, width, stageHeight])
    .style("width", `${width}px`)
    .style("height", `${stageHeight}px`)
    .style("shape-rendering", "geometricPrecision")
    .style("text-rendering", "geometricPrecision")
    .attr("role", "img")
    .attr("aria-label", "Circle packing chart of IPL team valuations");




  const defs = svg.append("defs");
  const glow = defs
    .append("filter")
    .attr("id", "softGlow")
    .attr("height", "120%")
    .attr("width", "120%")
    .attr("x", "-10%")
    .attr("y", "-10%");

  glow
    .append("feGaussianBlur")
    .attr("in", "SourceAlpha")
    .attr("stdDeviation", 12)
    .attr("result", "blur");

  glow
    .append("feColorMatrix")
    .attr("in", "blur")
    .attr("type", "matrix")
    .attr(
      "values",
      "0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0.35 0"
    )
    .attr("result", "shadow");

  const glowMerge = glow.append("feMerge");
  glowMerge.append("feMergeNode").attr("in", "shadow");
  glowMerge.append("feMergeNode").attr("in", "SourceGraphic");

  if (ballSymbol) {
    defs
      .append("symbol")
      .attr("id", "cricket-ball")
      .attr("viewBox", ballSymbol.viewBox)
      .html(ballSymbol.markup);
  }

  const baseExponent = 0.8;
  const baseMinBoost = 40;
  const minValuation = d3.min(data, (d) => d.Valuation_USD_Mn);
  const minTargetWeight = Math.pow(minValuation, baseExponent) + baseMinBoost;

  const exponent = 0.9;
  const minBoostBase = 20;
  const sizeScale = 1.25;
  const scaleFactor =
    ((minTargetWeight - minBoostBase) / Math.pow(minValuation, exponent)) *
    sizeScale;
  const minBoost = Math.max(
    0,
    minTargetWeight - Math.pow(minValuation, exponent) * scaleFactor
  );

  const root = d3
    .hierarchy({ children: data })
    .sum((d) => minBoost + Math.pow(d.Valuation_USD_Mn, exponent) * scaleFactor)
    .sort((a, b) => b.value - a.value);

  d3.pack().size([width, plotHeight]).padding(24)(root);

  const centerX = width / 2;
  const centerY = plotHeight / 2;
  const plotGroup = svg
    .append("g")
    .attr("class", "plot-group")
    .attr("transform", `translate(0,${plotOffsetY}) rotate(-90 ${centerX} ${centerY})`);
  const startX = width * 0.85;
  const startY = -60;
  const nodesData = root.leaves();
  const gap = 1;
  const leftX = width * 0.33;
  const rightX = width * 0.67;
  const columns = [
    { x: leftX, y: gap },
    { x: rightX, y: gap },
  ];

  nodesData
    .slice()
    .sort((a, b) => b.r - a.r)
    .forEach((d) => {
      const col = columns[0].y <= columns[1].y ? columns[0] : columns[1];
      d.layoutX = col.x;
      d.layoutY = col.y + d.r;
      col.y = d.layoutY + d.r + gap;
    });

  const getX = (d) => d.layoutX ?? d.x;
  const getY = (d) => d.layoutY ?? d.y;

  const nodes = plotGroup
    .append("g")
    .selectAll("g")
    .data(root.leaves())
    .join("g")
    .attr("transform", () => `translate(${startX},${startY}) scale(0.2)`);

  nodes
    .transition()
    .duration(520)
    .delay((d, i) => i * 35)
    .ease(d3.easeCubicIn)
    .attr("transform", () => `translate(${centerX},${centerY}) scale(0.6)`)
    .transition()
    .duration(520)
    .ease(d3.easeBackOut)
    .attr("transform", (d) => `translate(${getX(d)},${getY(d)}) scale(1.05)`)
    .transition()
    .duration(220)
    .ease(d3.easeQuadOut)
    .attr("transform", (d) => `translate(${getX(d)},${getY(d)}) scale(1)`);

  const ballScale = 1.25; // increase size (try 1.1 to 1.5)
  nodes
    .append("use")
    .attr("class", "bubble")
    .attr("href", "#cricket-ball")
    .attr("width", (d) => d.r * 2 * ballScale)
    .attr("height", (d) => d.r * 2 * ballScale)
    .attr("x", (d) => -d.r * ballScale)
    .attr("y", (d) => -d.r * ballScale)
    .attr("filter", "url(#softGlow)")
    .style("color", (d) => colorMap.get(d.data.Team) || "#9aa0a6")
    .on("mouseenter", (event, d) => {
      const color = colorMap.get(d.data.Team) || "#9aa0a6";
      tooltip
        .style("opacity", 1)
        .style("left", `${event.offsetX}px`)
        .style("top", `${event.offsetY}px`)
        .style("--team-color", color)
        .html(
          `<div class="tooltip-header">
            <span class="tooltip-swatch" style="background:${color}"></span>
            <div>
              <div class="tooltip-title">${d.data.Team}</div>
              <div class="tooltip-sub">Rank #${d.data.Rank} in valuation</div>
            </div>
          </div>
          <div class="tooltip-grid">
            <div class="tooltip-row"><span class="label">Valuation</span><span class="value">$${formatNumber(
              d.data.Valuation_USD_Mn
            )}M</span></div>
            <div class="tooltip-row"><span class="label">Share of total</span><span class="value">${formatPercent(
              d.data.Percentage_of_Total
            )}</span></div>
          </div>`
        );
    })
    .on("mousemove", (event) => {
      tooltip.style("left", `${event.offsetX}px`).style("top", `${event.offsetY}px`);
    })
    .on("mouseleave", () => {
      tooltip.style("opacity", 0);
    });

  const labelGroup = plotGroup
    .append("g")
    .attr("class", "edge-labels")
    .style("opacity", 0);

  const labelNodes = labelGroup
    .selectAll("g")
    .data(root.leaves().filter((d) => d.r >= 28))
    .join("g")
    .attr("transform", (d) => `translate(${getX(d)},${getY(d)})`);

  const fontScale = d3
    .scaleLinear()
    .domain(d3.extent(root.leaves(), (d) => d.data.Valuation_USD_Mn))
    .range([11, 22]);

  labelNodes.each(function (d) {
    const color = colorMap.get(d.data.Team) || "#9aa0a6";
    const textColor = color;
    const isTopTwo = d.data.Rank <= 2;
    const specialTeams = new Set([
      "rajasthan royals",
      "royal challengers bengaluru",
      "royal challengers bangalore",
    ]);
    const isSpecial = specialTeams.has(d.data.Team.toLowerCase());
    const teamRadius = isSpecial
      ? d.r - Math.max(16, d.r * 0.14)
      : d.r - Math.max(1, d.r * 0.03);
    const valueRadius = isTopTwo
      ? d.r - Math.max(20, d.r * 0.18)
      : d.r - Math.max(8, d.r * 0.05);
    const idBase = d.data.Team.replace(/[^a-z0-9]+/gi, "-").toLowerCase();
    const teamId = `arc-team-${idBase}`;
    const valueId = `arc-value-${idBase}`;
    const fontSize = Math.max(7, fontScale(d.data.Valuation_USD_Mn));
    const label = d3.select(this);

    defs
      .append("path")
      .attr("id", teamId)
      .attr("d", `M ${-teamRadius} 0 A ${teamRadius} ${teamRadius} 0 0 0 ${teamRadius} 0`);

    defs
      .append("path")
      .attr("id", valueId)
      .attr(
        "d",
        `M ${-valueRadius} 0 A ${valueRadius} ${valueRadius} 0 0 1 ${valueRadius} 0`
      );

    label
      .append("text")
      .attr("class", "edge-label")
      .attr("fill", textColor)
      .attr("font-size", fontSize)
      .append("textPath")
      .attr("href", `#${teamId}`)
      .attr("startOffset", "50%")
      .text(d.data.Team);

    label
      .append("text")
      .attr("class", "edge-label")
      .attr("fill", textColor)
      .attr("font-size", Math.max(12, fontSize + 2))
      .append("textPath")
      .attr("href", `#${valueId}`)
      .attr("startOffset", "50%")
      .text(`$${formatNumber(d.data.Valuation_USD_Mn)}M`);
  });

  labelGroup.transition().duration(500).delay(1200).style("opacity", 1);

  return () => {
    nodes.interrupt();
    labelGroup.interrupt();

    nodes.attr("transform", () => `translate(${startX},${startY}) scale(0.2)`);
    labelGroup.style("opacity", 0);

    nodes
      .transition()
      .duration(520)
      .delay((d, i) => i * 35)
      .ease(d3.easeCubicIn)
      .attr("transform", () => `translate(${centerX},${centerY}) scale(0.6)`)
      .transition()
      .duration(520)
      .ease(d3.easeBackOut)
      .attr("transform", (d) => `translate(${getX(d)},${getY(d)}) scale(1.05)`)
      .transition()
      .duration(220)
      .ease(d3.easeQuadOut)
      .attr("transform", (d) => `translate(${getX(d)},${getY(d)}) scale(1)`);

    labelGroup.transition().duration(500).delay(1200).style("opacity", 1);
  };
};

Promise.all([
  d3.csv("ipl_team_valuations.csv", (d) => ({
    Team: d.Team.trim(),
    Valuation_USD_Mn: +String(d.Valuation_USD_Mn).trim(),
    Percentage_of_Total: +String(d.Percentage_of_Total).trim(),
  })),
  d3.text("ipl_team_colors.txt"),
  d3.text("Cricket_Ball.svg"),
]).then(([data, colorsText, ballText]) => {
  const colorMap = parseColors(colorsText);
  const ballSymbol = buildBallSymbol(ballText);
  const ranked = [...data].sort((a, b) => b.Valuation_USD_Mn - a.Valuation_USD_Mn);
  const rankMap = new Map(ranked.map((d, i) => [d.Team, i + 1]));
  data.forEach((d) => {
    d.Rank = rankMap.get(d.Team);
  });
  let replay = draw(data, colorMap, ballSymbol);

  const observer = new ResizeObserver(() => {
    replay = draw(data, colorMap, ballSymbol);
  });
  observer.observe(chartEl);

  const startAutoPlay = () => {
    if (animationTimer) clearInterval(animationTimer);
    animationTimer = setInterval(() => {
      if (replay) replay();
    }, 10000);
  };

  playButton.addEventListener("click", () => {
    if (replay) replay();
    startAutoPlay();
  });

  startAutoPlay();
});
