# the registry formulas match the reviewed snapshot

    Code
      cat(yaml::as.yaml(latex))
    Output
      bernoulli:
        pdf: P(X = x) = p^{x} (1 - p)^{1 - x}, \quad x \in \{0, 1\}
        cdf: F(x) = \begin{cases} 0, & x < 0 \\ 1 - p, & 0 \le x < 1 \\ 1, & x \ge 1 \end{cases}
        quantile:
          expr: \begin{cases} 0, & 0 \le u \le 1 - p \\ 1, & 1 - p < u \le 1 \end{cases}
        mean:
          expr: p
        variance:
          expr: p (1 - p)
        skewness:
        - expr: \frac{1 - 2p}{\sqrt{p (1 - p)}}
          condition: 0 < p < 1
        - status: undefined
          condition: p \in \{0, 1\}
        excess_kurtosis:
        - expr: \frac{1 - 6p (1 - p)}{p (1 - p)}
          condition: 0 < p < 1
        - status: undefined
          condition: p \in \{0, 1\}
        raw_moment:
        - expr: p
          condition: j > 0
        - expr: '1'
          condition: j = 0
        - status: infinite
          condition: j < 0, p < 1
        - expr: '1'
          condition: j < 0, p = 1
        mgf:
          expr: 1 - p + p e^{t}
        cf:
          expr: 1 - p + p e^{i t}
        pgf:
          expr: 1 - p + p z
        median:
        - expr: '0'
          condition: p < \frac{1}{2}
        - expr: '[0, 1]'
          condition: p = \frac{1}{2}
        - expr: '1'
          condition: p > \frac{1}{2}
        mode:
        - expr: '0'
          condition: p < \frac{1}{2}
        - expr: \{0, 1\}
          condition: p = \frac{1}{2}
        - expr: '1'
          condition: p > \frac{1}{2}
        entropy:
        - expr: -(1 - p) \log(1 - p) - p \log p
          condition: 0 < p < 1
        - expr: '0'
          condition: p \in \{0, 1\}
      beta:
        pdf: f(x) = \frac{x^{\alpha - 1} (1 - x)^{\beta - 1}}{B(\alpha, \beta)}, \quad 0
          < x < 1
        cdf: F(x) = I_{x}(\alpha, \beta)
        quantile:
          expr: ~
        mean:
          expr: \frac{\alpha}{\alpha + \beta}
        variance:
          expr: \frac{\alpha \beta}{(\alpha + \beta)^{2} (\alpha + \beta + 1)}
        skewness:
          expr: \frac{2 (\beta - \alpha) \sqrt{\alpha + \beta + 1}}{(\alpha + \beta + 2)
            \sqrt{\alpha \beta}}
        excess_kurtosis:
          expr: \frac{6 \left[(\alpha - \beta)^{2} (\alpha + \beta + 1) - \alpha \beta (\alpha
            + \beta + 2)\right]}{\alpha \beta (\alpha + \beta + 2)(\alpha + \beta + 3)}
        raw_moment:
        - expr: \frac{B(\alpha + j, \beta)}{B(\alpha, \beta)}
          condition: j > -\alpha
        - status: infinite
          condition: j \le -\alpha
        mgf:
          expr: '{}_{1}F_{1}(\alpha; \alpha + \beta; t)'
        cf:
          expr: '{}_{1}F_{1}(\alpha; \alpha + \beta; i t)'
        median:
        - expr: \frac{1}{2}
          condition: \alpha = \beta
        - expr: 1 - 2^{-1/\beta}
          condition: \alpha = 1, \beta \ne 1
        - expr: 2^{-1/\alpha}
          condition: \beta = 1, \alpha \ne 1
        - expr: ~
          condition: \alpha \ne \beta, \alpha \ne 1, \beta \ne 1
        mode:
        - expr: \frac{\alpha - 1}{\alpha + \beta - 2}
          condition: \alpha > 1, \beta > 1
        - expr: '0'
          condition: \alpha \le 1, \beta > 1
        - expr: '0'
          condition: \alpha < 1, \beta = 1
        - expr: '1'
          condition: \alpha > 1, \beta \le 1
        - expr: '1'
          condition: \alpha = 1, \beta < 1
        - expr: \{0, 1\}
          condition: \alpha < 1, \beta < 1
        - expr: '[0, 1]'
          condition: \alpha = 1, \beta = 1
        entropy:
          expr: \log B(\alpha, \beta) - (\alpha - 1) \psi(\alpha) - (\beta - 1) \psi(\beta)
            + (\alpha + \beta - 2) \psi(\alpha + \beta)
      betabinomial:
        pdf: P(X = x) = \binom{n}{x} \frac{B(x + \alpha, n - x + \beta)}{B(\alpha, \beta)},
          \quad x = 0, 1, \ldots, n
        cdf: F(x) = \sum_{j=0}^{\lfloor x \rfloor} \binom{n}{j} \frac{B(j + \alpha, n -
          j + \beta)}{B(\alpha, \beta)}
        quantile:
          expr: ~
        mean:
          expr: \frac{n \alpha}{\alpha + \beta}
        variance:
          expr: \frac{n \alpha \beta (\alpha + \beta + n)}{(\alpha + \beta)^{2} (\alpha
            + \beta + 1)}
        skewness:
          expr: \frac{(\alpha + \beta + 2n)(\beta - \alpha)}{\alpha + \beta + 2} \sqrt{\frac{1
            + \alpha + \beta}{n \alpha \beta (n + \alpha + \beta)}}
        excess_kurtosis:
          expr: \frac{(\alpha + \beta)^{2} (1 + \alpha + \beta)}{n \alpha \beta (\alpha
            + \beta + 2)(\alpha + \beta + 3)(\alpha + \beta + n)} \left[(\alpha + \beta)(\alpha
            + \beta - 1 + 6n) + 3 \alpha \beta (n - 2) + 6n^{2} - \frac{3 \alpha \beta n
            (6 - n)}{\alpha + \beta} - \frac{18 \alpha \beta n^{2}}{(\alpha + \beta)^{2}}\right]
            - 3
        mgf:
          expr: '{}_{2}F_{1}\left(-n, \alpha; \alpha + \beta; 1 - e^{t}\right)'
        cf:
          expr: '{}_{2}F_{1}\left(-n, \alpha; \alpha + \beta; 1 - e^{i t}\right)'
        pgf:
          expr: '{}_{2}F_{1}\left(-n, \alpha; \alpha + \beta; 1 - z\right)'
        median:
          expr: ~
        mode:
          expr: ~
        entropy:
          expr: ~
      binomial:
        pdf: P(X = x) = \binom{n}{x} p^{x} (1 - p)^{n - x}, \quad x = 0, 1, \ldots, n
        cdf: F(x) = \sum_{k=0}^{\lfloor x \rfloor} \binom{n}{k} p^{k} (1 - p)^{n - k}
        quantile:
          expr: ~
        mean:
          expr: n p
        variance:
          expr: n p (1 - p)
        skewness:
        - expr: \frac{1 - 2p}{\sqrt{n p (1 - p)}}
          condition: 0 < p < 1
        - status: undefined
          condition: p \in \{0, 1\}
        excess_kurtosis:
        - expr: \frac{1 - 6p (1 - p)}{n p (1 - p)}
          condition: 0 < p < 1
        - status: undefined
          condition: p \in \{0, 1\}
        mgf:
          expr: \left(1 - p + p e^{t}\right)^{n}
        cf:
          expr: \left(1 - p + p e^{i t}\right)^{n}
        pgf:
          expr: (1 - p + p z)^{n}
        median:
        - expr: n p
          condition: n p \in \mathbb{Z}
        - expr: ~
          condition: n p \notin \mathbb{Z}
        mode:
        - expr: \lfloor (n + 1) p \rfloor
          condition: (n + 1) p \notin \{1, 2, \ldots, n + 1\}
        - expr: \{(n + 1) p - 1, (n + 1) p\}
          condition: (n + 1) p \in \{1, 2, \ldots, n\}
        - expr: 'n'
          condition: p = 1
        entropy:
          expr: ~
      cauchy:
        pdf: f(x) = \frac{1}{\pi \gamma \left[1 + \left(\frac{x - x_0}{\gamma}\right)^2\right]}
        cdf: F(x) = \frac{1}{2} + \frac{1}{\pi} \arctan\left(\frac{x - x_0}{\gamma}\right)
        quantile:
          expr: x_0 + \gamma \tan\left(\pi \left(u - \frac{1}{2}\right)\right)
        mean:
          status: undefined
        variance:
          status: undefined
        skewness:
          status: undefined
        excess_kurtosis:
          status: undefined
        mgf:
        - expr: '1'
          condition: t = 0
        - status: infinite
          condition: t \ne 0
        cf:
          expr: e^{i x_0 t - \gamma |t|}
        median:
          expr: x_0
        mode:
          expr: x_0
        entropy:
          expr: \log(4 \pi \gamma)
      chisq:
        pdf: f(x) = \frac{x^{k/2 - 1} e^{-x/2}}{2^{k/2} \Gamma(k/2)}, \quad x > 0
        cdf: F(x) = \frac{\gamma(k/2, x/2)}{\Gamma(k/2)}
        quantile:
          expr: ~
        mean:
          expr: k
        variance:
          expr: 2k
        skewness:
          expr: \sqrt{\frac{8}{k}}
        excess_kurtosis:
          expr: \frac{12}{k}
        raw_moment:
        - expr: \frac{2^{j} \Gamma(k/2 + j)}{\Gamma(k/2)}
          condition: j > -\frac{k}{2}
        - status: infinite
          condition: j \le -\frac{k}{2}
        mgf:
        - expr: (1 - 2t)^{-k/2}
          condition: t < \frac{1}{2}
        - status: infinite
          condition: t \ge \frac{1}{2}
        cf:
          expr: (1 - 2 i t)^{-k/2}
        median:
          expr: ~
        mode:
        - expr: k - 2
          condition: k \ge 2
        - expr: '0'
          condition: k < 2
        entropy:
          expr: \frac{k}{2} + \log\left(2 \Gamma\left(\frac{k}{2}\right)\right) + \left(1
            - \frac{k}{2}\right) \psi\left(\frac{k}{2}\right)
      discrete_uniform:
        pdf: P(X = x) = \frac{1}{b - a + 1}, \quad x = a, a + 1, \ldots, b
        cdf: F(x) = \frac{\lfloor x \rfloor - a + 1}{b - a + 1}, \quad a \le x < b
        quantile:
        - expr: a - 1 + \lceil u (b - a + 1) \rceil
          condition: 0 < u \le 1
        - expr: a
          condition: u = 0
        mean:
          expr: \frac{a + b}{2}
        variance:
          expr: \frac{(b - a + 1)^{2} - 1}{12}
        skewness:
          expr: '0'
        excess_kurtosis:
          expr: -\frac{6 \left[(b - a + 1)^{2} + 1\right]}{5 \left[(b - a + 1)^{2} - 1\right]}
        mgf:
        - expr: \frac{e^{a t} - e^{(b + 1) t}}{(b - a + 1)\left(1 - e^{t}\right)}
          condition: t \ne 0
        - expr: '1'
          condition: t = 0
        cf:
        - expr: \frac{e^{i a t} - e^{i (b + 1) t}}{(b - a + 1)\left(1 - e^{i t}\right)}
          condition: \frac{t}{2 \pi} \notin \mathbb{Z}
        - expr: '1'
          condition: \frac{t}{2 \pi} \in \mathbb{Z}
        pgf:
        - expr: \frac{z^{a} - z^{b + 1}}{(b - a + 1)(1 - z)}
          condition: z \ne 1, a \ge 0
        - expr: \frac{z^{a} - z^{b + 1}}{(b - a + 1)(1 - z)}
          condition: z \ne 0, z \ne 1, a < 0
        - expr: '1'
          condition: z = 1
        - status: undefined
          condition: z = 0, a < 0
        median:
        - expr: \frac{a + b}{2}
          condition: b - a \in \{2, 4, 6, \ldots\}
        - expr: \left[\frac{a + b - 1}{2}, \frac{a + b + 1}{2}\right]
          condition: b - a \in \{1, 3, 5, \ldots\}
        mode:
          expr: \{a, a + 1, \ldots, b\}
        entropy:
          expr: \log(b - a + 1)
      exponential:
        pdf: f(x) = \lambda e^{-\lambda x}, \quad x \ge 0
        cdf: F(x) = 1 - e^{-\lambda x}, \quad x \ge 0
        quantile:
          expr: -\frac{\log(1 - u)}{\lambda}
        mean:
          expr: \frac{1}{\lambda}
        variance:
          expr: \frac{1}{\lambda^{2}}
        skewness:
          expr: '2'
        excess_kurtosis:
          expr: '6'
        raw_moment:
        - expr: \frac{\Gamma(1 + j)}{\lambda^{j}}
          condition: j > -1
        - status: infinite
          condition: j \le -1
        mgf:
        - expr: \frac{\lambda}{\lambda - t}
          condition: t < \lambda
        - status: infinite
          condition: t \ge \lambda
        cf:
          expr: \frac{\lambda}{\lambda - i t}
        median:
          expr: \frac{\log 2}{\lambda}
        mode:
          expr: '0'
        entropy:
          expr: 1 - \log \lambda
      f:
        pdf: f(x) = \frac{1}{B(d_1/2, d_2/2)} \left(\frac{d_1}{d_2}\right)^{d_1/2} x^{d_1/2
          - 1} \left(1 + \frac{d_1}{d_2} x\right)^{-(d_1 + d_2)/2}, \quad x > 0
        cdf: F(x) = I_{d_1 x / (d_1 x + d_2)}\left(\frac{d_1}{2}, \frac{d_2}{2}\right)
        quantile:
          expr: ~
        mean:
        - expr: \frac{d_2}{d_2 - 2}
          condition: d_2 > 2
        - status: infinite
          condition: d_2 \le 2
        variance:
        - expr: \frac{2 d_2^{2} (d_1 + d_2 - 2)}{d_1 (d_2 - 2)^{2} (d_2 - 4)}
          condition: d_2 > 4
        - status: infinite
          condition: 2 < d_2 \le 4
        - status: undefined
          condition: d_2 \le 2
        skewness:
        - expr: \frac{(2 d_1 + d_2 - 2) \sqrt{8 (d_2 - 4)}}{(d_2 - 6) \sqrt{d_1 (d_1 + d_2
            - 2)}}
          condition: d_2 > 6
        - status: infinite
          condition: 4 < d_2 \le 6
        - status: undefined
          condition: d_2 \le 4
        excess_kurtosis:
        - expr: \frac{12 \left[d_1 (5 d_2 - 22)(d_1 + d_2 - 2) + (d_2 - 4)(d_2 - 2)^{2}\right]}{d_1
            (d_2 - 6)(d_2 - 8)(d_1 + d_2 - 2)}
          condition: d_2 > 8
        - status: infinite
          condition: 4 < d_2 \le 8
        - status: undefined
          condition: d_2 \le 4
        raw_moment:
        - expr: \left(\frac{d_2}{d_1}\right)^{j} \frac{\Gamma\left(\frac{d_1}{2} + j\right)
            \Gamma\left(\frac{d_2}{2} - j\right)}{\Gamma\left(\frac{d_1}{2}\right) \Gamma\left(\frac{d_2}{2}\right)}
          condition: -\frac{d_1}{2} < j < \frac{d_2}{2}
        - status: infinite
          condition: j \ge \frac{d_2}{2}
        - status: infinite
          condition: j \le -\frac{d_1}{2}
        mgf:
        - expr: ~
          condition: t \le 0
        - status: infinite
          condition: t > 0
        cf:
          expr: \frac{\Gamma\left(\frac{d_1 + d_2}{2}\right)}{\Gamma\left(\frac{d_2}{2}\right)}
            U\left(\frac{d_1}{2}, 1 - \frac{d_2}{2}, -\frac{d_2}{d_1} i t\right)
        median:
          expr: ~
        mode:
        - expr: \frac{d_1 - 2}{d_1} \cdot \frac{d_2}{d_2 + 2}
          condition: d_1 > 2
        - expr: '0'
          condition: d_1 \le 2
        entropy:
          expr: \log\frac{d_2}{d_1} + \log B\left(\frac{d_1}{2}, \frac{d_2}{2}\right) +
            \left(1 - \frac{d_1}{2}\right) \psi\left(\frac{d_1}{2}\right) - \left(1 + \frac{d_2}{2}\right)
            \psi\left(\frac{d_2}{2}\right) + \frac{d_1 + d_2}{2} \psi\left(\frac{d_1 + d_2}{2}\right)
      gamma:
        pdf: f(x) = \frac{\beta^{\alpha}}{\Gamma(\alpha)} x^{\alpha-1} e^{-\beta x}, \quad
          x > 0
        cdf: F(x) = \frac{\gamma(\alpha, \beta x)}{\Gamma(\alpha)}
        quantile:
          expr: ~
        mean:
          expr: \frac{\alpha}{\beta}
        variance:
          expr: \frac{\alpha}{\beta^{2}}
        skewness:
          expr: \frac{2}{\sqrt{\alpha}}
        excess_kurtosis:
          expr: \frac{6}{\alpha}
        raw_moment:
        - expr: \frac{\Gamma(\alpha + j)}{\beta^{j} \Gamma(\alpha)}
          condition: j > -\alpha
        - status: infinite
          condition: j \le -\alpha
        mgf:
        - expr: \left(1 - \frac{t}{\beta}\right)^{-\alpha}
          condition: t < \beta
        - status: infinite
          condition: t \ge \beta
        cf:
          expr: \left(1 - \frac{i t}{\beta}\right)^{-\alpha}
        median:
          expr: ~
        mode:
        - expr: \frac{\alpha - 1}{\beta}
          condition: \alpha \ge 1
        - expr: '0'
          condition: \alpha < 1
        entropy:
          expr: \alpha - \log \beta + \log \Gamma(\alpha) + (1 - \alpha) \psi(\alpha)
      geometric:
        pdf: P(X = x) = p (1 - p)^{x}, \quad x = 0, 1, 2, \ldots \ \text{(failures before
          the first success)}
        cdf: F(x) = 1 - (1 - p)^{\lfloor x \rfloor + 1}, \quad x \ge 0
        quantile:
        - expr: \left\lceil \frac{\log(1 - u)}{\log(1 - p)} \right\rceil - 1
          condition: 0 < u < 1, p < 1
        - expr: '0'
          condition: u = 0, p < 1
        - expr: \infty
          condition: u = 1, p < 1
        - expr: '0'
          condition: p = 1
        mean:
          expr: \frac{1 - p}{p}
        variance:
          expr: \frac{1 - p}{p^{2}}
        skewness:
        - expr: \frac{2 - p}{\sqrt{1 - p}}
          condition: p < 1
        - status: undefined
          condition: p = 1
        excess_kurtosis:
        - expr: 6 + \frac{p^{2}}{1 - p}
          condition: p < 1
        - status: undefined
          condition: p = 1
        mgf:
        - expr: \frac{p}{1 - (1 - p) e^{t}}
          condition: t < -\log(1 - p)
        - status: infinite
          condition: t \ge -\log(1 - p)
        cf:
          expr: \frac{p}{1 - (1 - p) e^{i t}}
        pgf:
        - expr: \frac{p}{1 - (1 - p) z}
          condition: '|z| < \frac{1}{1 - p}'
        - status: infinite
          condition: z \ge \frac{1}{1 - p}
        - status: undefined
          condition: z \le -\frac{1}{1 - p}
        median:
        - expr: \left\lceil -\frac{\log 2}{\log(1 - p)} \right\rceil - 1
          condition: 0 < p < 1, \frac{\log 2}{\log(1 - p)} \notin \mathbb{Z}
        - expr: \left[-\frac{\log 2}{\log(1 - p)} - 1, -\frac{\log 2}{\log(1 - p)}\right]
          condition: 0 < p < 1, \frac{\log 2}{\log(1 - p)} \in \mathbb{Z}
        - expr: '0'
          condition: p = 1
        mode:
          expr: '0'
        entropy:
        - expr: \frac{-(1 - p) \log(1 - p) - p \log p}{p}
          condition: 0 < p < 1
        - expr: '0'
          condition: p = 1
      hypergeometric:
        pdf: P(X = x) = \frac{\binom{m}{x} \binom{n}{k - x}}{\binom{m + n}{k}}, \quad \max(0,
          k - n) \le x \le \min(k, m)
        cdf: F(x) = \sum_{j = \max(0, k - n)}^{\lfloor x \rfloor} \frac{\binom{m}{j} \binom{n}{k
          - j}}{\binom{m + n}{k}}
        quantile:
          expr: ~
        mean:
          expr: \frac{k m}{m + n}
        variance:
          expr: \frac{k m n (m + n - k)}{(m + n)^{2} (m + n - 1)}
        skewness:
        - expr: \frac{(n - m)(m + n - 2k) \sqrt{m + n - 1}}{(m + n - 2) \sqrt{k m n (m +
            n - k)}}
          condition: m + n > 2, k < m + n
        - expr: '0'
          condition: m + n = 2, k = 1
        - status: undefined
          condition: k = m + n
        excess_kurtosis:
        - expr: \frac{(m + n - 1)(m + n)^{2} \left[(m + n)(m + n + 1) - 6 m n - 6 k (m +
            n - k)\right] + 6 k m n (m + n - k)(5 (m + n) - 6)}{k m n (m + n - k)(m + n
            - 2)(m + n - 3)}
          condition: m + n > 3, k < m + n
        - expr: '-2'
          condition: m + n = 2, k = 1
        - expr: -\frac{3}{2}
          condition: m + n = 3, k < 3
        - status: undefined
          condition: k = m + n
        mgf:
        - expr: \frac{\binom{n}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k, -m; n - k
            + 1; e^{t}\right)
          condition: k \le n
        - expr: e^{k t} \frac{\binom{m}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k, -n;
            m - k + 1; e^{-t}\right)
          condition: n < k \le m
        - expr: e^{m t} \frac{\binom{n}{m + n - k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(k
            - m - n, -m; k - m + 1; e^{-t}\right)
          condition: k > n, k > m
        cf:
        - expr: \frac{\binom{n}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k, -m; n - k
            + 1; e^{i t}\right)
          condition: k \le n
        - expr: e^{i k t} \frac{\binom{m}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k,
            -n; m - k + 1; e^{-i t}\right)
          condition: n < k \le m
        - expr: e^{i m t} \frac{\binom{n}{m + n - k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(k
            - m - n, -m; k - m + 1; e^{-i t}\right)
          condition: k > n, k > m
        pgf:
        - expr: \frac{\binom{n}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k, -m; n - k
            + 1; z\right)
          condition: k \le n
        - expr: z^{k} \frac{\binom{m}{k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(-k, -n;
            m - k + 1; z^{-1}\right)
          condition: n < k \le m, z \ne 0
        - expr: z^{m} \frac{\binom{n}{m + n - k}}{\binom{m + n}{k}} \, {}_{2}F_{1}\left(k
            - m - n, -m; k - m + 1; z^{-1}\right)
          condition: k > n, k > m, z \ne 0
        - expr: '0'
          condition: k > n, z = 0
        median:
          expr: ~
        mode:
        - expr: \left\lfloor \frac{(k + 1)(m + 1)}{m + n + 2} \right\rfloor
          condition: \frac{(k + 1)(m + 1)}{m + n + 2} \notin \mathbb{Z}
        - expr: \left\{\frac{(k + 1)(m + 1)}{m + n + 2} - 1, \frac{(k + 1)(m + 1)}{m + n
            + 2}\right\}
          condition: \frac{(k + 1)(m + 1)}{m + n + 2} \in \mathbb{Z}
        entropy:
          expr: ~
      laplace:
        pdf: f(x) = \frac{1}{2b} \exp\left(-\frac{|x - \mu|}{b}\right)
        cdf: F(x) = \begin{cases} \frac{1}{2} e^{(x - \mu)/b}, & x < \mu \\ 1 - \frac{1}{2}
          e^{-(x - \mu)/b}, & x \ge \mu \end{cases}
        quantile:
          expr: \mu - b \, \operatorname{sgn}\left(u - \frac{1}{2}\right) \log\left(1 -
            2 \left|u - \frac{1}{2}\right|\right)
        mean:
          expr: \mu
        variance:
          expr: 2 b^{2}
        skewness:
          expr: '0'
        excess_kurtosis:
          expr: '3'
        mgf:
        - expr: \frac{e^{\mu t}}{1 - b^{2} t^{2}}
          condition: '|t| < \frac{1}{b}'
        - status: infinite
          condition: '|t| \ge \frac{1}{b}'
        cf:
          expr: \frac{e^{i \mu t}}{1 + b^{2} t^{2}}
        median:
          expr: \mu
        mode:
          expr: \mu
        entropy:
          expr: 1 + \log(2 b)
      logistic:
        pdf: f(x) = \frac{e^{-(x - \mu)/s}}{s \left(1 + e^{-(x - \mu)/s}\right)^2}
        cdf: F(x) = \frac{1}{1 + e^{-(x - \mu)/s}}
        quantile:
          expr: \mu + s \log\frac{u}{1 - u}
        mean:
          expr: \mu
        variance:
          expr: \frac{s^{2} \pi^{2}}{3}
        skewness:
          expr: '0'
        excess_kurtosis:
          expr: \frac{6}{5}
        mgf:
        - expr: e^{\mu t} B(1 - s t, 1 + s t)
          condition: '|t| < \frac{1}{s}'
        - status: infinite
          condition: '|t| \ge \frac{1}{s}'
        cf:
        - expr: e^{i \mu t} \frac{\pi s t}{\sinh(\pi s t)}
          condition: t \ne 0
        - expr: '1'
          condition: t = 0
        median:
          expr: \mu
        mode:
          expr: \mu
        entropy:
          expr: \log s + 2
      lognormal:
        pdf: f(x) = \frac{1}{x \sigma \sqrt{2\pi}} \exp\left\{-\frac{(\log x - \mu)^2}{2\sigma^2}\right\},
          \quad x > 0
        cdf: F(x) = \Phi\left(\frac{\log x - \mu}{\sigma}\right)
        quantile:
          expr: \exp\left(\mu + \sigma \Phi^{-1}(u)\right)
        mean:
          expr: e^{\mu + \sigma^{2}/2}
        variance:
          expr: \left(e^{\sigma^{2}} - 1\right) e^{2\mu + \sigma^{2}}
        skewness:
          expr: \left(e^{\sigma^{2}} + 2\right) \sqrt{e^{\sigma^{2}} - 1}
        excess_kurtosis:
          expr: e^{4\sigma^{2}} + 2 e^{3\sigma^{2}} + 3 e^{2\sigma^{2}} - 6
        raw_moment:
          expr: e^{j \mu + j^{2} \sigma^{2}/2}
        mgf:
        - expr: ~
          condition: t \le 0
        - status: infinite
          condition: t > 0
        cf:
          expr: ~
        median:
          expr: e^{\mu}
        mode:
          expr: e^{\mu - \sigma^{2}}
        entropy:
          expr: \mu + \frac{1}{2} \log\left(2 \pi e \sigma^{2}\right)
      negbinomial:
        pdf: P(X = x) = \frac{\Gamma(x + r)}{\Gamma(r)\, x!} p^{r} (1 - p)^{x}, \quad x
          = 0, 1, 2, \ldots \ \text{(failures before the } r\text{-th success)}
        cdf: F(x) = I_{p}(r, \lfloor x \rfloor + 1)
        quantile:
          expr: ~
        mean:
          expr: \frac{r (1 - p)}{p}
        variance:
          expr: \frac{r (1 - p)}{p^{2}}
        skewness:
        - expr: \frac{2 - p}{\sqrt{r (1 - p)}}
          condition: p < 1
        - status: undefined
          condition: p = 1
        excess_kurtosis:
        - expr: \frac{6}{r} + \frac{p^{2}}{r (1 - p)}
          condition: p < 1
        - status: undefined
          condition: p = 1
        mgf:
        - expr: \left(\frac{p}{1 - (1 - p) e^{t}}\right)^{r}
          condition: t < -\log(1 - p)
        - status: infinite
          condition: t \ge -\log(1 - p)
        cf:
          expr: \left(\frac{p}{1 - (1 - p) e^{i t}}\right)^{r}
        pgf:
        - expr: \left(\frac{p}{1 - (1 - p) z}\right)^{r}
          condition: '|z| < \frac{1}{1 - p}'
        - status: infinite
          condition: z \ge \frac{1}{1 - p}
        - status: undefined
          condition: z \le -\frac{1}{1 - p}
        median:
          expr: ~
        mode:
        - expr: \max\left(0, \left\lfloor \frac{(r - 1)(1 - p)}{p} \right\rfloor\right)
          condition: \frac{(r - 1)(1 - p)}{p} \notin \{1, 2, 3, \ldots\}
        - expr: \left\{\frac{(r - 1)(1 - p)}{p} - 1, \frac{(r - 1)(1 - p)}{p}\right\}
          condition: \frac{(r - 1)(1 - p)}{p} \in \{1, 2, 3, \ldots\}
        entropy:
          expr: ~
      normal:
        pdf: f(x) = \frac{1}{\sigma\sqrt{2\pi}} \exp\left\{-\frac{(x-\mu)^2}{2\sigma^2}\right\}
        cdf: F(x) = \Phi\left(\frac{x-\mu}{\sigma}\right)
        quantile:
          expr: \mu + \sigma \Phi^{-1}(u)
        mean:
          expr: \mu
        variance:
          expr: \sigma^{2}
        skewness:
          expr: '0'
        excess_kurtosis:
          expr: '0'
        mgf:
          expr: \exp\left(\mu t + \frac{\sigma^{2} t^{2}}{2}\right)
        cf:
          expr: \exp\left(i \mu t - \frac{\sigma^{2} t^{2}}{2}\right)
        median:
          expr: \mu
        mode:
          expr: \mu
        entropy:
          expr: \frac{1}{2} \log\left(2 \pi e \sigma^{2}\right)
      poisson:
        pdf: P(X = x) = \frac{e^{-\lambda} \lambda^{x}}{x!}, \quad x = 0, 1, 2, \ldots
        cdf: F(x) = \sum_{k=0}^{\lfloor x \rfloor} \frac{e^{-\lambda} \lambda^{k}}{k!}
        quantile:
          expr: ~
        mean:
          expr: \lambda
        variance:
          expr: \lambda
        skewness:
          expr: \frac{1}{\sqrt{\lambda}}
        excess_kurtosis:
          expr: \frac{1}{\lambda}
        mgf:
          expr: \exp\left\{\lambda \left(e^{t} - 1\right)\right\}
        cf:
          expr: \exp\left\{\lambda \left(e^{i t} - 1\right)\right\}
        pgf:
          expr: \exp\left\{\lambda (z - 1)\right\}
        median:
          expr: ~
        mode:
        - expr: \lfloor \lambda \rfloor
          condition: \lambda \notin \mathbb{Z}
        - expr: \{\lambda - 1, \lambda\}
          condition: \lambda \in \mathbb{Z}
        entropy:
          expr: ~
      student_t:
        pdf: f(x) = \frac{\Gamma\left(\frac{\nu + 1}{2}\right)}{\sqrt{\nu\pi}\, \Gamma\left(\frac{\nu}{2}\right)}
          \left(1 + \frac{x^2}{\nu}\right)^{-(\nu + 1)/2}
        cdf: F(x) = 1 - \frac{1}{2} I_{\nu / (x^2 + \nu)}\left(\frac{\nu}{2}, \frac{1}{2}\right),
          \quad x \ge 0
        quantile:
          expr: ~
        mean:
        - expr: '0'
          condition: \nu > 1
        - status: undefined
          condition: \nu \le 1
        variance:
        - expr: \frac{\nu}{\nu - 2}
          condition: \nu > 2
        - status: infinite
          condition: 1 < \nu \le 2
        - status: undefined
          condition: \nu \le 1
        skewness:
        - expr: '0'
          condition: \nu > 3
        - status: undefined
          condition: \nu \le 3
        excess_kurtosis:
        - expr: \frac{6}{\nu - 4}
          condition: \nu > 4
        - status: infinite
          condition: 2 < \nu \le 4
        - status: undefined
          condition: \nu \le 2
        raw_moment:
        - expr: '0'
          condition: j \in \{1, 3, 5, \ldots\}, j < \nu
        - expr: \frac{\nu^{j/2} \, \Gamma\left(\frac{j + 1}{2}\right) \Gamma\left(\frac{\nu
            - j}{2}\right)}{\sqrt{\pi} \, \Gamma\left(\frac{\nu}{2}\right)}
          condition: j \in \{0, 2, 4, \ldots\}, j < \nu
        - status: undefined
          condition: j \in \{1, 3, 5, \ldots\}, j \ge \nu
        - status: infinite
          condition: j \in \{2, 4, 6, \ldots\}, j \ge \nu
        - status: undefined
          condition: j \notin \mathbb{Z}
        - status: infinite
          condition: j \in \{-2, -4, -6, \ldots\}
        - status: undefined
          condition: j \in \{-1, -3, -5, \ldots\}
        mgf:
        - expr: '1'
          condition: t = 0
        - status: infinite
          condition: t \ne 0
        cf:
        - expr: \frac{K_{\nu/2}\left(\sqrt{\nu} |t|\right) \left(\sqrt{\nu} |t|\right)^{\nu/2}}{\Gamma(\nu/2)
            \, 2^{\nu/2 - 1}}
          condition: t \ne 0
        - expr: '1'
          condition: t = 0
        median:
          expr: '0'
        mode:
          expr: '0'
        entropy:
          expr: \frac{\nu + 1}{2} \left[\psi\left(\frac{\nu + 1}{2}\right) - \psi\left(\frac{\nu}{2}\right)\right]
            + \log\left(\sqrt{\nu} \, B\left(\frac{\nu}{2}, \frac{1}{2}\right)\right)
      uniform:
        pdf: f(x) = \frac{1}{b - a}, \quad a \le x \le b
        cdf: F(x) = \frac{x - a}{b - a}, \quad a \le x \le b
        quantile:
          expr: a + u (b - a)
        mean:
          expr: \frac{a + b}{2}
        variance:
          expr: \frac{(b - a)^{2}}{12}
        skewness:
          expr: '0'
        excess_kurtosis:
          expr: -\frac{6}{5}
        raw_moment:
        - expr: \frac{b^{j + 1} - a^{j + 1}}{(j + 1)(b - a)}
          condition: j \in \{0, 1, 2, \ldots\}
        - expr: \frac{b^{j + 1} - a^{j + 1}}{(j + 1)(b - a)}
          condition: a \ge 0, j > -1, j \notin \mathbb{Z}
        - expr: \frac{b^{j + 1} - a^{j + 1}}{(j + 1)(b - a)}
          condition: a > 0, j < -1
        - expr: \frac{\log b - \log a}{b - a}
          condition: a > 0, j = -1
        - status: infinite
          condition: a = 0, j \le -1
        - status: undefined
          condition: a < 0, j \notin \mathbb{Z}
        - expr: \frac{b^{j + 1} - a^{j + 1}}{(j + 1)(b - a)}
          condition: b < 0, j \in \{-2, -3, -4, \ldots\}
        - expr: \frac{\log(-b) - \log(-a)}{b - a}
          condition: b < 0, j = -1
        - status: infinite
          condition: a < 0, b \ge 0, j \in \{-2, -4, -6, \ldots\}
        - status: undefined
          condition: a < 0 < b, j \in \{-1, -3, -5, \ldots\}
        - expr: -\infty
          condition: b = 0, j \in \{-1, -3, -5, \ldots\}
        mgf:
        - expr: \frac{e^{b t} - e^{a t}}{t (b - a)}
          condition: t \ne 0
        - expr: '1'
          condition: t = 0
        cf:
        - expr: \frac{e^{i b t} - e^{i a t}}{i t (b - a)}
          condition: t \ne 0
        - expr: '1'
          condition: t = 0
        median:
          expr: \frac{a + b}{2}
        mode:
          expr: '[a, b]'
        entropy:
          expr: \log(b - a)
      weibull:
        pdf: f(x) = \frac{k}{\lambda} \left(\frac{x}{\lambda}\right)^{k - 1} e^{-(x/\lambda)^{k}},
          \quad x \ge 0
        cdf: F(x) = 1 - e^{-(x/\lambda)^{k}}, \quad x \ge 0
        quantile:
          expr: \lambda \left(-\log(1 - u)\right)^{1/k}
        mean:
          expr: \lambda \Gamma\left(1 + \frac{1}{k}\right)
        variance:
          expr: \lambda^{2} \left[\Gamma\left(1 + \frac{2}{k}\right) - \Gamma^{2}\left(1
            + \frac{1}{k}\right)\right]
        skewness:
          expr: \frac{\Gamma\left(1 + \frac{3}{k}\right) - 3 \Gamma\left(1 + \frac{1}{k}\right)
            \Gamma\left(1 + \frac{2}{k}\right) + 2 \Gamma^{3}\left(1 + \frac{1}{k}\right)}{\left[\Gamma\left(1
            + \frac{2}{k}\right) - \Gamma^{2}\left(1 + \frac{1}{k}\right)\right]^{3/2}}
        excess_kurtosis:
          expr: \frac{\Gamma\left(1 + \frac{4}{k}\right) - 4 \Gamma\left(1 + \frac{1}{k}\right)
            \Gamma\left(1 + \frac{3}{k}\right) + 6 \Gamma^{2}\left(1 + \frac{1}{k}\right)
            \Gamma\left(1 + \frac{2}{k}\right) - 3 \Gamma^{4}\left(1 + \frac{1}{k}\right)}{\left[\Gamma\left(1
            + \frac{2}{k}\right) - \Gamma^{2}\left(1 + \frac{1}{k}\right)\right]^{2}} -
            3
        raw_moment:
        - expr: \lambda^{j} \Gamma\left(1 + \frac{j}{k}\right)
          condition: j > -k
        - status: infinite
          condition: j \le -k
        mgf:
        - expr: ~
          condition: k > 1
        - expr: \frac{1}{1 - \lambda t}
          condition: k = 1, t < \frac{1}{\lambda}
        - status: infinite
          condition: k = 1, t \ge \frac{1}{\lambda}
        - expr: ~
          condition: k < 1, t \le 0
        - status: infinite
          condition: k < 1, t > 0
        cf:
          expr: ~
        median:
          expr: \lambda (\log 2)^{1/k}
        mode:
        - expr: \lambda \left(\frac{k - 1}{k}\right)^{1/k}
          condition: k > 1
        - expr: '0'
          condition: k \le 1
        entropy:
          expr: \gamma_{\mathrm{E}} \left(1 - \frac{1}{k}\right) + \log\frac{\lambda}{k}
            + 1

