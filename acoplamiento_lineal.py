"""
Sistema lineal de cabreo acoplado entre dos personas.

    dC1/dt = -lambda1 * C1 + mu1 * C2
    dC2/dt = -lambda2 * C2 + mu2 * C1

El signo de Delta = lambda1*lambda2 - mu1*mu2 determina el régimen:
    Delta > 0  =>  calma (ambos -> 0)
    Delta < 0  =>  escalada (crecimiento exponencial)
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import solve_ivp

# ── Fuente serif ──────────────────────────────────────────────────────────────
plt.rcParams.update({
    "font.family": "serif",
    "font.size": 9,
    "axes.titlesize": 9,
    "axes.labelsize": 8,
    "legend.fontsize": 7,
    "xtick.labelsize": 7,
    "ytick.labelsize": 7,
})

# ── Parámetros de estilo ──────────────────────────────────────────────────────
GRISES = ["0.15", "0.45", "0.65"]   # uno por trayectoria

# ── Parámetros de cada régimen ────────────────────────────────────────────────
CALMA = dict(
    lambda1=0.15, lambda2=0.15, mu1=0.08, mu2=0.08,
    t_fin=80,
    condiciones=[(3.0, 0.5), (1.0, 1.0), (0.5, 3.0)],
    titulo="Régimen de calma  ($\\mu_1\\mu_2 < \\lambda_1\\lambda_2$)",
    ylim_cero=True,
)

ESCALADA = dict(
    lambda1=0.15, lambda2=0.15, mu1=0.25, mu2=0.25,
    t_fin=40,
    condiciones=[(1.0, 0.2), (0.5, 0.5), (2.0, 1.0)],
    titulo="Régimen de escalada  ($\\mu_1\\mu_2 > \\lambda_1\\lambda_2$)",
    ylim_cero=False,
)


# ── Sistema de EDOs ───────────────────────────────────────────────────────────
def sistema(t, y, lambda1, lambda2, mu1, mu2):
    C1, C2 = y
    dC1 = -lambda1 * C1 + mu1 * C2
    dC2 = -lambda2 * C2 + mu2 * C1
    return [dC1, dC2]


# ── Integración y dibujo de un régimen en un eje dado ─────────────────────────
def dibujar_regimen(ax, regimen):
    p = regimen
    t_eval = np.linspace(0, p["t_fin"], 2000)

    for (C1_0, C2_0), gris in zip(p["condiciones"], GRISES):
        sol = solve_ivp(
            sistema,
            t_span=(0, p["t_fin"]),
            y0=[C1_0, C2_0],
            t_eval=t_eval,
            args=(p["lambda1"], p["lambda2"], p["mu1"], p["mu2"]),
            method="RK45",
            rtol=1e-9,
            atol=1e-11,
        )
        etiqueta = f"$C_1$, $(C_1^0,C_2^0)=({C1_0},{C2_0})$"
        ax.plot(sol.t, sol.y[0], color=gris, ls="-",  lw=1.2, label=etiqueta)
        etiqueta2 = f"$C_2$, $(C_1^0,C_2^0)=({C1_0},{C2_0})$"
        ax.plot(sol.t, sol.y[1], color=gris, ls="--", lw=1.2, label=etiqueta2)

    ax.set_title(p["titulo"])
    ax.set_xlabel("tiempo")
    ax.set_ylabel("$C(t)$")
    ax.legend(loc="best", framealpha=0.6)

    if p["ylim_cero"]:
        ax.set_ylim(bottom=0)


# ── Main ──────────────────────────────────────────────────────────────────────
def main():
    fig, (ax_calma, ax_escalada) = plt.subplots(1, 2, figsize=(10, 4))

    dibujar_regimen(ax_calma,    CALMA)
    dibujar_regimen(ax_escalada, ESCALADA)

    plt.tight_layout()

    fig.savefig("acoplamiento_lineal.pdf", dpi=300)
    fig.savefig("acoplamiento_lineal.png", dpi=150)
    print("Guardado: acoplamiento_lineal.pdf  y  acoplamiento_lineal.png")


if __name__ == "__main__":
    main()
