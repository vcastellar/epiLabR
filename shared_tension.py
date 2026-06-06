import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

def shared_tension(t, y, lam, alpha, sigma, mu, nu):
    C1, C2, T = y
    dC1 = -lam * C1 + alpha * C2 - sigma * T * C1
    dC2 = -lam * C2 + alpha * C1 - sigma * T * C2
    dT  = mu * (C1 + C2) - nu * T
    return [dC1, dC2, dT]

t_span = (0, 150)
t_eval = np.linspace(0, 150, 3000)

scenarios = [
    {
        "label": "S1 — Resolución\n(α < λ, el contagio no compensa el olvido)",
        "params": dict(lam=0.15, alpha=0.05, sigma=0.10, mu=0.50, nu=1.00),
        "init": [10, 2, 0],
    },
    {
        "label": "S2 — Equilibrio crónico, convergencia monótona\n(α > λ, ν > 4(α−λ))",
        "params": dict(lam=0.10, alpha=0.30, sigma=0.05, mu=0.30, nu=2.00),
        "init": [10, 2, 0],
    },
    {
        "label": "S3 — Equilibrio crónico, convergencia oscilatoria\n(α > λ, ν < 4(α−λ))",
        "params": dict(lam=0.10, alpha=0.30, sigma=0.05, mu=0.30, nu=0.10),
        "init": [10, 2, 0],
    },
    {
        "label": "S4 — Oscilaciones con inicio asimétrico pronunciado\n(α > λ, ν ≪ 4(α−λ))",
        "params": dict(lam=0.05, alpha=0.40, sigma=0.03, mu=0.20, nu=0.08),
        "init": [20, 1, 0],
    },
    {
        "label": "S5 — Tensión compartida dominante\n(μ grande, retroalimentación fuerte)",
        "params": dict(lam=0.10, alpha=0.25, sigma=0.08, mu=1.00, nu=0.50),
        "init": [8, 6, 0],
    },
]

fig, axes = plt.subplots(len(scenarios), 1, figsize=(10, 4 * len(scenarios)))

for ax, sc in zip(axes, scenarios):
    p = sc["params"]
    sol = solve_ivp(
        shared_tension, t_span, sc["init"], t_eval=t_eval,
        args=(p["lam"], p["alpha"], p["sigma"], p["mu"], p["nu"]),
        method="RK45", rtol=1e-8, atol=1e-10,
    )

    lam, alpha, nu = p["lam"], p["alpha"], p["nu"]
    osc_threshold = 4 * (alpha - lam) if alpha > lam else None
    eq_info = ""
    if alpha > lam:
        C_star = nu * (alpha - lam) / (2 * p["sigma"] * p["mu"])
        eq_info = f"  C* ≈ {C_star:.2f},  umbral oscilación ν* = {osc_threshold:.3f},  ν = {nu:.3f}"

    ax.plot(sol.t, sol.y[0], label="C₁ (individuo 1)", color="#e05c5c", lw=2)
    ax.plot(sol.t, sol.y[1], label="C₂ (individuo 2)", color="#5c7ae0", lw=2)
    ax.plot(sol.t, sol.y[2], label="T (tensión compartida)", color="#2ca02c", lw=1.5, ls="--")
    ax.set_title(sc["label"] + eq_info, fontsize=10, loc="left")
    ax.set_xlabel("Tiempo")
    ax.set_ylabel("Cabreo / Tensión")
    ax.legend(fontsize=9)
    ax.set_xlim(0, 150)
    ax.grid(alpha=0.3)

fig.suptitle("Modelo de tensión compartida", fontsize=14, fontweight="bold", y=1.01)
plt.tight_layout()
plt.savefig("/home/user/epiLabR/shared_tension.png", dpi=150, bbox_inches="tight")
plt.show()
print("Guardado en shared_tension.png")
