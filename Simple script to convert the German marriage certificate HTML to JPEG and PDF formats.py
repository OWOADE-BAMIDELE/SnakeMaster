#!/usr/bin/env python3
"""
Simple script to convert the German marriage certificate HTML to JPEG and PDF formats.
Uses alternative libraries that are easier to install.
"""

import os
import sys
from pathlib import Path

def convert_with_weasyprint(html_file, output_file):
    """Convert HTML to PDF using WeasyPrint."""
    try:
        from weasyprint import HTML
        print(f"Converting HTML to PDF using WeasyPrint: {output_file}")
        
        HTML(html_file).write_pdf(output_file)
        print(f"✓ PDF created successfully: {output_file}")
        return True
        
    except ImportError:
        print("WeasyPrint not available. Install with: pip install weasyprint")
        return False
    except Exception as e:
        print(f"✗ Error creating PDF with WeasyPrint: {e}")
        return False

def convert_with_playwright(html_file, output_file):
    """Convert HTML to JPEG using Playwright."""
    try:
        from playwright.sync_api import sync_playwright
        print(f"Converting HTML to JPEG using Playwright: {output_file}")
        
        with sync_playwright() as p:
            browser = p.chromium.launch()
            page = browser.new_page()
            
            # Load the HTML file
            html_path = Path(html_file).absolute().as_uri()
            page.goto(html_path)
            
            # Wait for page to load and take screenshot
            page.wait_for_load_state('networkidle')
            page.screenshot(path=output_file, full_page=True)
            
            browser.close()
        
        print(f"✓ JPEG created successfully: {output_file}")
        return True
        
    except ImportError:
        print("Playwright not available. Install with: pip install playwright")
        return False
    except Exception as e:
        print(f"✗ Error creating JPEG with Playwright: {e}")
        return False

def convert_with_imgkit(html_file, output_file):
    """Convert HTML to JPEG using imgkit."""
    try:
        import imgkit
        print(f"Converting HTML to JPEG using imgkit: {output_file}")
        
        options = {
            'format': 'jpg',
            'encoding': 'UTF-8',
            'width': 1200,
            'height': 800
        }
        
        imgkit.from_file(html_file, output_file, options=options)
        print(f"✓ JPEG created successfully: {output_file}")
        return True
        
    except ImportError:
        print("imgkit not available. Install with: pip install imgkit")
        return False
    except Exception as e:
        print(f"✗ Error creating JPEG with imgkit: {e}")
        return False

def main():
    """Main conversion function."""
    html_file = "german_marriage_certificate.html"
    
    # Check if HTML file exists
    if not os.path.exists(html_file):
        print(f"Error: {html_file} not found!")
        sys.exit(1)
    
    print("Starting conversion of German Marriage Certificate...")
    print("=" * 50)
    
    # Try different conversion methods
    jpeg_file = "german_marriage_certificate.jpg"
    pdf_file = "german_marriage_certificate.pdf"
    
    # Try to create JPEG
    jpeg_success = False
    if convert_with_playwright(html_file, jpeg_file):
        jpeg_success = True
    elif convert_with_imgkit(html_file, jpeg_file):
        jpeg_success = True
    else:
        print("Could not create JPEG with available libraries")
    
    # Try to create PDF
    pdf_success = convert_with_weasyprint(html_file, pdf_file)
    
    print("=" * 50)
    print("Conversion Summary:")
    print(f"HTML: {html_file} ✓")
    print(f"JPEG: {jpeg_file} {'✓' if jpeg_success else '✗'}")
    print(f"PDF: {pdf_file} {'✓' if pdf_success else '✗'}")
    
    if jpeg_success and pdf_success:
        print("\n🎉 All conversions completed successfully!")
    elif jpeg_success or pdf_success:
        print(f"\n⚠️  Partial success. Check which files were created.")
    else:
        print("\n❌ All conversions failed. Check the error messages above.")
        print("\nTry installing one of these packages:")
        print("pip install weasyprint playwright imgkit")

if __name__ == "__main__":
    main()














