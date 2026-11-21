// The header Prompt link is a cursor:// deeplink so the desktop app opens
// with the starter sentence filled in. A phone tap on that scheme does
// nothing: the mobile browser has no app registered for it. Phones go to
// https://cursor.com/agents instead. The iOS app opens those links, and
// a phone without the app opens the same page in the browser.

const PHONE_PROMPT = "https://cursor.com/agents";

function isPhone() {
  const ua = navigator.userAgent || "";
  if (/Android|iPhone|iPad|iPod/i.test(ua)) return true;
  const touchOnly = window.matchMedia("(pointer: coarse)").matches
    && !window.matchMedia("(hover: hover)").matches;
  // iPadOS reports a desktop user agent. It still has no cursor:// handler.
  return touchOnly || (navigator.maxTouchPoints > 1
    && !window.matchMedia("(hover: hover)").matches);
}

function retargetPromptLinks() {
  if (!isPhone()) return;
  const links = document.querySelectorAll('a[href^="cursor://anysphere.cursor-deeplink/prompt"]');
  for (const link of links) link.href = PHONE_PROMPT;
}

retargetPromptLinks();
