# rasblocking — GitHub Pages

This branch only hosts the interactive map of our INFORMS RAS 2026 blocking plan:
https://annicenajafi.github.io/rasblocking/map/

The map shows the final submission (3rd of 83 teams), routed over the release v2.1 network: corridor
flows, block trains with the shipments they carry, shipment routes, yards, and costs from the official
v2.1 metric. It is one self-contained HTML file; the only external pieces are deck.gl (unpkg) and Google Fonts.

Data: RAS 2026 Problem Solving Competition release v2.1 (RAS 2026 Problem Committee); demand derived
from BTS/FAF freight-flow data. The R package lives on the `master` branch.
