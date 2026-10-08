document.addEventListener('DOMContentLoaded', function() {

    // ==========================================
    // 1. MODO OSCURO / CLARO (Theme Toggle)
    // ==========================================
    const themeToggle = document.getElementById('theme-toggle');
    const currentTheme = localStorage.getItem('theme') || 'light';
    
    document.documentElement.setAttribute('data-theme', currentTheme);
    updateThemeUI(currentTheme);

    if (themeToggle) {
        themeToggle.addEventListener('click', function(e) {
            e.preventDefault();
            const theme = document.documentElement.getAttribute('data-theme');
            const newTheme = theme === 'light' ? 'dark' : 'light';
            
            document.documentElement.setAttribute('data-theme', newTheme);
            localStorage.setItem('theme', newTheme);
            updateThemeUI(newTheme);
        });
    }

    function updateThemeUI(theme) {
        if (!themeToggle) return;
        const textSpan = themeToggle.querySelector('span');
        const svgIcon = themeToggle.querySelector('svg');
        
        if (theme === 'dark') {
            if (textSpan) textSpan.textContent = 'Modo Claro';
            if (svgIcon) {
                // SVG de Sol para retornar a Modo Claro
                svgIcon.innerHTML = '<path d="M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zM2 13h2c.55 0 1-.45 1-1s-.45-1-1-1H2c-.55 0-1 .45-1 1s.45 1 1 1zm18 0h2c.55 0 1-.45 1-1s-.45-1-1-1h-2c-.55 0-1 .45-1 1s.45 1 1 1zM11 2v2c0 .55.45 1 1 1s1-.45 1-1V2c0-.55-.45-1-1-1s-1 .45-1 1zm0 18v2c0 .55.45 1 1 1s1-.45 1-1v-2c0-.55-.45-1-1-1s-1 .45-1 1z"/>';
            }
        } else {
            if (textSpan) textSpan.textContent = 'Modo Oscuro';
            if (svgIcon) {
                // SVG de Luna para activar Modo Oscuro
                svgIcon.innerHTML = '<path d="M12 3c-4.97 0-9 4.03-9 9s4.03 9 9 9 9-4.03 9-9c0-.46-.04-.92-.1-1.36-.98 1.37-2.58 2.26-4.4 2.26-2.98 0-5.4-2.42-5.4-5.4 0-1.81.89-3.42 2.26-4.4C12.92 3.04 12.46 3 12 3z"/>';
            }
        }
    }

    // ==========================================
    // 2. CONTROL GLOBAL DE VISIBILIDAD DE CÓDIGO
    // ==========================================
    const codeToggle = document.getElementById('code-toggle');
    let codeVisible = true;

    if (codeToggle) {
        codeToggle.addEventListener('click', function(e) {
            e.preventDefault();
            codeVisible = !codeVisible;
            
            // Alterna la clase en el body para control CSS
            document.body.classList.toggle('code-hidden', !codeVisible);
            
            const textSpan = codeToggle.querySelector('span');
            if (textSpan) {
                textSpan.textContent = codeVisible ? 'Ocultar Código' : 'Mostrar Código';
            }
        });
    }

    // ==========================================
    // 3. VISOR DEL MODAL PARA scraper.R
    // ==========================================
    const viewScraperBtn = document.getElementById('view-scraper-btn');
    if (viewScraperBtn) {
        viewScraperBtn.addEventListener('click', function(e) {
            e.preventDefault();
            const scraperContentEl = document.getElementById('scraper-code-content');
            const modalBody = document.getElementById('scraperModalBody');
            
            if (scraperContentEl && modalBody) {
                modalBody.innerHTML = scraperContentEl.innerHTML;
                
                const modalEl = document.getElementById('scraperModal');
                if (modalEl && typeof bootstrap !== 'undefined') {
                    const scraperModal = bootstrap.Modal.getOrCreateInstance(modalEl);
                    scraperModal.show();
                } else if (modalEl) {
                    // Fallback directo por CSS si Bootstrap JS no ha terminado de inicializar
                    modalEl.classList.add('show');
                    modalEl.style.display = 'block';
                    document.body.classList.add('modal-open');
                }
            } else {
                alert('El código de scraper.R no está disponible en la página.');
            }
        });
    }
});
