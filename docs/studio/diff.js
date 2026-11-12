// Line-by-line comparison of two texts (Myers' O(ND) difference algorithm).

// Beyond this many changed lines the texts are reported as too different;
// it bounds the memory the algorithm needs to a few megabytes.
const MAX_DISTANCE = 1500;

function splitLines(text) {
  const lines = text.split('\n');
  if (lines.length && lines[lines.length - 1] === '') lines.pop();
  return lines;
}

// Returns a list of { type: 'same' | 'add' | 'remove', line, a, b } where a
// and b are 1-based line numbers in the old and new text, or null when the
// texts are too different to compare line by line.
export function diffLines(oldText, newText) {
  const a = splitLines(oldText);
  const b = splitLines(newText);
  let start = 0;
  while (start < a.length && start < b.length && a[start] === b[start]) start += 1;
  let endA = a.length;
  let endB = b.length;
  while (endA > start && endB > start && a[endA - 1] === b[endB - 1]) {
    endA -= 1;
    endB -= 1;
  }
  const middle = myers(a.slice(start, endA), b.slice(start, endB));
  if (middle === null) return null;
  const result = [];
  for (let i = 0; i < start; i += 1) result.push({ type: 'same', line: a[i], a: i + 1, b: i + 1 });
  for (const item of middle) {
    result.push({
      type: item.type,
      line: item.line,
      a: item.a === null ? null : item.a + start + 1,
      b: item.b === null ? null : item.b + start + 1,
    });
  }
  for (let i = endA; i < a.length; i += 1) {
    result.push({ type: 'same', line: a[i], a: i + 1, b: endB + (i - endA) + 1 });
  }
  return result;
}

function myers(a, b) {
  const n = a.length;
  const m = b.length;
  if (n === 0) return b.map((line, j) => ({ type: 'add', line, a: null, b: j }));
  if (m === 0) return a.map((line, i) => ({ type: 'remove', line, a: i, b: null }));
  const max = Math.min(n + m, MAX_DISTANCE);
  const offset = max + 1;
  let v = new Int32Array(2 * max + 3);
  const trace = [];
  let found = -1;
  for (let d = 0; d <= max && found < 0; d += 1) {
    trace.push(v.slice());
    for (let k = -d; k <= d; k += 2) {
      let x;
      if (k === -d || (k !== d && v[offset + k - 1] < v[offset + k + 1])) {
        x = v[offset + k + 1];
      } else {
        x = v[offset + k - 1] + 1;
      }
      let y = x - k;
      while (x < n && y < m && a[x] === b[y]) {
        x += 1;
        y += 1;
      }
      v[offset + k] = x;
      if (x >= n && y >= m) {
        found = d;
        break;
      }
    }
  }
  if (found < 0) return null;
  // Walk the trace backwards to recover the edit script.
  const script = [];
  let x = n;
  let y = m;
  for (let d = found; d > 0; d -= 1) {
    const previous = trace[d];
    const k = x - y;
    let previousK;
    if (k === -d || (k !== d && previous[offset + k - 1] < previous[offset + k + 1])) {
      previousK = k + 1;
    } else {
      previousK = k - 1;
    }
    const previousX = previous[offset + previousK];
    const previousY = previousX - previousK;
    while (x > previousX && y > previousY) {
      x -= 1;
      y -= 1;
      script.push({ type: 'same', line: a[x], a: x, b: y });
    }
    if (x === previousX) {
      y -= 1;
      script.push({ type: 'add', line: b[y], a: null, b: y });
    } else {
      x -= 1;
      script.push({ type: 'remove', line: a[x], a: x, b: null });
    }
  }
  while (x > 0 && y > 0) {
    x -= 1;
    y -= 1;
    script.push({ type: 'same', line: a[x], a: x, b: y });
  }
  return script.reverse();
}

// Group a diff into hunks, hiding long runs of unchanged lines.
export function hunks(diff, context = 3) {
  const keep = new Uint8Array(diff.length);
  diff.forEach((item, index) => {
    if (item.type === 'same') return;
    const from = Math.max(0, index - context);
    const to = Math.min(diff.length - 1, index + context);
    for (let i = from; i <= to; i += 1) keep[i] = 1;
  });
  const result = [];
  let index = 0;
  while (index < diff.length) {
    const begin = index;
    const kept = keep[index];
    while (index < diff.length && keep[index] === kept) index += 1;
    if (kept) result.push({ type: 'lines', items: diff.slice(begin, index) });
    else result.push({ type: 'skip', count: index - begin });
  }
  return result;
}

export function summarize(diff) {
  let added = 0;
  let removed = 0;
  for (const item of diff) {
    if (item.type === 'add') added += 1;
    else if (item.type === 'remove') removed += 1;
  }
  return { added, removed };
}
