# What a Companies House Address Tells You About Local Ownership — and What It Doesn't

Research note prepared 2026-06-15. Prompted by the [Veedol case](veedol_company_history.md):
a Rotherham-registered firm that is actually Indian-owned, via a shelf company. That
raised a broader point — **a registered address in Sheffield does not mean a
Sheffield-owned business**, and conversely **many genuinely Sheffield-owned or
-operated businesses never appear at a Sheffield address in Companies House at all.**

This doc summarises the main structural forms a firm can take while still showing a
Sheffield registered address, the mechanisms that hide true ownership, and the converse
cases that make CH location data an unreliable proxy for "local economy".

## TL;DR for the dataset

A registered office is a **statutory mailbox**, not evidence of ownership, control, or
even of where the business trades. To infer local economic ownership from Companies
House you have to treat the registered address as a weak signal and layer on company
type, the company-number prefix, group/parent links, and PSC data — each of which has
its own blind spots described below.

---

## 1. Three different things that get conflated

| Concept | What it is | Where it lives in CH |
|---------|-----------|----------------------|
| **Registered office** | A legal service address for statutory mail. Can be an accountant, solicitor, formation agent, virtual office, or the group HQ. | `RegAddress.*` |
| **Trading address(es)** | Where the business actually operates. May be several sites, in different places. | **Usually not in CH at all** |
| **Ownership / control** | Who ultimately owns and controls the company. | Shareholders (not fully public), PSC register, parent links |

These three can all point to different cities — or different countries. The Sheffield
registered office of a company can sit above a business that trades only in London and
is owned in Mumbai.

---

## 2. Legal entity types you can find at a Sheffield address

All of the following can carry a Sheffield registered office:

- **Private company limited by shares (Ltd)** — the default. Owned by shareholders;
  shareholdings are only partially public (annual confirmation statement snapshot, not
  live).
- **Public limited company (PLC)** — can raise capital from the public; if listed, it's
  exempt from the PSC regime (see §5).
- **Private company limited by guarantee** — no share capital; "owned" by members not
  shareholders. Used by charities, clubs, membership/professional bodies, social
  enterprises, management companies. Ownership question is different (no equity).
- **Limited Liability Partnership (LLP)** — prefix `OC`/`SO`/`NC`. Owned by members;
  common for professional services.
- **Limited Partnership (LP)** — prefix `LP`/`SL`/`NL`. General + limited partners.
- **Scottish Limited Partnership (SLP)** — prefix `SL`. Has separate legal personality
  (unlike E&W LPs), which historically made it a favoured opacity vehicle; brought into
  a PSC-style disclosure regime in 2017.
- **Community Interest Company (CIC)** — normal company number, plus an asset lock and
  the CIC Regulator. Social-purpose, but still a company.
- **Unlimited company** — no limited liability; in some cases exempt from publishing
  accounts, so reveals *less* than a normal Ltd.
- **Right-to-Manage / flat-management / commonhold companies** — large volume, usually
  registered at a residential address or a managing agent. Not "businesses" in an
  economic sense; significant noise in any location-based analysis.
- **Dormant / non-trading / holding companies, and name-protection shells** — exist on
  the register but do nothing locally.

---

## 3. Company-number prefixes — jurisdiction & type at a glance

The `CompanyNumber` prefix is one of the few reliable structural signals in the raw
data. A Sheffield trading business could legitimately appear under several of these:

| Prefix | Meaning |
|--------|---------|
| *(none, 8 digits)* | England & Wales company |
| `SC` | Scottish company |
| `NI` / `R0` | Northern Ireland company (post- / pre-partition) |
| `OC` / `SO` / `NC` | LLP — England & Wales / Scotland / NI |
| `LP` / `SL` / `NL` | Limited Partnership — E&W / Scotland / NI |
| `FC` | Overseas company (the foreign parent itself) |
| `BR` | UK establishment (branch/place of business) of an overseas company |
| `SF` / `NF` | Overseas company registered in Scotland / NI (pre-2009) |
| `OE` | Overseas entity owning UK property (Register of Overseas Entities) |
| `CE` / `CS` | Charitable Incorporated Organisation — E&W / Scotland |
| `IP` / `SP` / `NP` | Industrial & Provident / registered society (now FCA-registered) |
| `IC` / `SI` / `NV` | ICVC (open-ended investment company) — E&W / Scotland / NI |
| `RC` / `SR` / `NR` | Royal Charter company |
| `AC` / `SA` / `NA` | Assurance company |
| `ZC` / `SZ` / `NZ` | Unregistered company |
| `SE` / `ES` / `EN` | European Company (Societas Europaea — legacy) |
| `GE` / `GS` / `GN` | European Economic Interest Grouping (legacy) |
| `SG` | Scottish Qualifying Partnership |
| `NO` | Northern Ireland credit union / I&P society |

A Scottish-registered (`SC`) company with a Sheffield trading site, or an `FC`/`BR`
overseas establishment operating in Sheffield, will both look "non-local" by number even
if the local activity is real — and vice versa.

---

## 4. Ownership & control structures that obscure "where the firm belongs"

This is the heart of the Veedol lesson. The same Sheffield registered office can sit on
top of very different ownership realities:

- **Subsidiary of a UK parent** — locally registered, but owned by a group whose HQ is
  anywhere in the UK. The parent may itself be a subsidiary (intermediate holdcos).
- **Subsidiary of an overseas parent** — UK-incorporated (so it looks domestic) but
  ultimately foreign-owned. This is the Veedol UK pattern.
- **Holding-company chains** — ownership routed through several layers of holdco, often
  for tax, financing or ring-fencing reasons.
- **Special Purpose Vehicles (SPVs)** — single-purpose companies common in property,
  infrastructure and project finance. A Sheffield building might be held by an SPV whose
  "business" is just that one asset.
- **Joint ventures** — co-owned by two or more unrelated groups; neither "owns" it
  outright.
- **Shelf companies** — bought ready-made, so the incorporation date and original name
  tell you nothing about the real business (see the [Veedol note](veedol_company_history.md)).
- **Nominee directors / nominee shareholders** — the named person holds the role/shares
  on behalf of an undisclosed beneficial owner. Legal owner ≠ beneficial owner.
- **Shares held via a trust** — beneficial ownership sits behind trustees; the trust's
  beneficiaries may not be visible.
- **Offshore ownership** — shares held by a BVI / Cayman / Jersey / Guernsey / Isle of
  Man entity. The PSC trail can stop at that entity (see §5).
- **Employee Ownership Trust (EOT)** — the company is owned by/for its employees via a
  trust (e.g. several Sheffield-region manufacturers). Genuinely "local" but the owner
  is a trust, not a person or parent.
- **Franchises** — the local outlet may be an independently-owned franchisee Ltd
  (locally owned, brand owned elsewhere) *or* a company-owned outlet (brand owns it
  directly). Same shopfront, opposite ownership answers.
- **Private-equity / VC-backed** — operating company owned through fund vehicles, often
  Jersey/Guernsey/Luxembourg limited partnerships. Control sits with a distant fund.

---

## 5. The PSC register — what it reveals, and its blind spots

The **Persons with Significant Control** regime (since 2016) is the main public window
onto control. A PSC is recorded if they meet any of: **>25% of shares**, **>25% of
voting rights**, the **right to appoint/remove a majority of the board**, otherwise
**significant influence or control**, or the same via a trust/firm.

What it does **not** reliably tell you:

- **Corporate PSCs (RLEs).** Control is often a **Relevant Legal Entity** — another
  company — not a named person. You must follow the chain. Only the *first* registrable
  RLE is recorded; you then have to look that entity up separately.
- **Listed-company exemption.** Companies with voting shares on the UK Official List (or
  certain EEA/US/Japan/Switzerland/Israel markets) are exempt from PSC (they report
  under the FCA's DTR5 instead). A subsidiary may just point to a listed parent.
- **The overseas trail goes cold.** A non-listed overseas company generally can't be an
  RLE, and isn't itself subject to the UK PSC regime, so the chain can dead-end at a
  foreign holdco whose own owners aren't in CH. (The Register of Overseas Entities, §6,
  only partially closes this, and only for property owners.)
- **"No PSC" statements.** Many companies file a statement that there is no registrable
  person — sometimes legitimately (diffuse ownership), sometimes via nominee/trust
  structuring. Veedol International filed exactly this.
- **Accuracy & verification.** Historically Companies House was a *register of what was
  filed, not what is true* — it did little verification. The Economic Crime and
  Corporate Transparency Act 2023 (ECCTA) is phasing in identity verification and
  query/reject powers, but older filings and the legacy gap remain.

---

## 6. The overseas dimension

A business operating in Sheffield but owned/run abroad can surface in CH in three quite
different ways — which matters because they look nothing alike in the data:

1. **UK-incorporated subsidiary** — ordinary 8-digit number, Sheffield registered
   office, looks fully domestic. (Veedol UK.) Foreign ownership only visible via PSC.
2. **UK establishment of an overseas company** — the foreign company gets an `FC`
   number and each UK branch a `BR` number (form OS IN01, with constitution + accounts).
   Discloses less than a UK company.
3. **Not in Companies House at all** — an overseas company can trade with UK customers
   without opening a registered "establishment", so it may have no CH presence despite
   real local activity.

Separately, the **Register of Overseas Entities** (`OE` IDs, since Aug 2022 under the
Economic Crime (Transparency and Enforcement) Act) forces overseas entities that *own UK
property* to declare beneficial owners — a different register from PSC, covering a
different population.

---

## 7. The converse — locally owned/operated firms that DON'T appear at a Sheffield address

Just as a Sheffield address overstates local ownership, CH also *under*-counts the local
economy. Genuinely Sheffield businesses that won't show a Sheffield CH address include:

- **Sole traders and ordinary (general) partnerships** — *not registered at Companies
  House at all*. Known to HMRC, invisible to CH. A large slice of the genuinely local
  economy (trades, freelancers, small shops, many partnerships) simply isn't there.
- **Charitable Incorporated Organisations (CIOs)** — on the Charity Commission /
  OSCR register, not CH.
- **Registered societies** (co-ops, community benefit societies, credit unions, many
  housing associations) — on the **FCA Mutuals Public Register**, not CH, though legacy
  `IP`/`SP`/`NP` numbers surface in some datasets.
- **Registered office held elsewhere** — a Sheffield-trading company whose registered
  office is its accountant or group HQ in Leeds, London, etc. It will geocode to the
  *wrong* place.
- **Branches/divisions of a company registered elsewhere** — a Sheffield site that is
  just a branch of a company registered in another city gets *no separate CH entry*; the
  parent appears only at its own registered office.
- **Trading names / brands** — not separate legal entities; the legal entity behind a
  Sheffield brand may be registered anywhere.

---

## 8. Implications for the Companies House dataset

For this project (firms geocoded to LA / ITL2 from registered postcode), the practical
upshot:

- **Registered address ≈ "where the paperwork sits", not "where the economy is".**
  Treat `localauthority_name` / `ITL221NM` as a noisy proxy for activity, not ownership.
- **Accountant/agent clustering.** Watch for many unrelated firms sharing one postcode —
  a sign of a registered-office service (accountant, formation agent, virtual office).
  These inflate counts at one location and should arguably be flagged/down-weighted.
- **Number prefix is a cheap, reliable filter.** `SC`/`NI`/`FC`/`BR`/`OE`/`OC` etc. let
  you separate jurisdictions, LLPs, overseas establishments and property-only entities
  before any geocoding.
- **Ownership needs PSC + parent links, not address.** "Home-grown vs externally owned"
  cannot be read off the registered office; it needs the PSC register (with the RLE
  chain followed) and, ideally, group-structure data — and even then the overseas trail
  can go cold.
- **Entity-type noise.** Guarantee companies, RTM/flat-management companies, dormant
  shells and SPVs all dilute any "real local business" measure; consider excluding or
  tagging them.
- **The missing economy.** Sole traders and ordinary partnerships are absent entirely —
  any "business density" measure from CH systematically undercounts exactly the most
  locally-rooted firms.

A defensible "locally owned Sheffield firm" definition therefore needs *several* signals
agreeing — registered address **and** a PSC who is a local individual (not a distant RLE)
**and** a sensible entity type — rather than the registered postcode alone.

---

## Sources

- [Companies House — URI customer guide (number prefixes)](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/809682/uniformResourceIdentifiersCustomerGuide.pdf)
- [Doorda — Company number prefixes defined](https://doorda.com/glossary/company-number-prefixes-defined/)
- [Brodies LLP — Overseas companies: do we need to register at Companies House?](https://brodies.com/insights/corporate/overseas-companies-do-we-need-to-register-at-companies-house/)
- [Travers Smith — The PSC regime: a guide for UK companies](https://www.traverssmith.com/knowledge/knowledge-container/the-psc-regime-a-guide-for-uk-companies-on-their-obligations/)
- [Inform Direct — What is a Relevant Legal Entity (RLE)?](https://www.informdirect.co.uk/company-records/what-is-a-relevant-legal-entity-rle/)
- [GOV.UK — Register of Overseas Entities](https://www.gov.uk/government/collections/register-of-overseas-entities)
- [ACCA — Companies House and overseas entities](https://www.accaglobal.com/uk/en/technical-activities/uk-tech/in-practice/2022/december/companies-house-overseas-entities.html)
