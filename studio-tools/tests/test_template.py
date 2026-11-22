"""The starter export contains the studio and the patches, and no pieces."""
import json
import tempfile
import unittest
from pathlib import Path

from support import REPO

from studio.paths import Studio
from studio.template_export import export_template


class ExportTemplateTests(unittest.TestCase):
    def test_starter_has_patches_and_an_empty_library(self):
        with tempfile.TemporaryDirectory() as temporary:
            destination = Path(temporary) / "starter"
            result = export_template(Studio(REPO), destination)
            self.assertEqual(result["compositions"], 0)
            catalog = json.loads((destination / "docs" / "compositions" / "catalog.json").read_text())
            self.assertEqual(catalog["compositions"], [])
            self.assertTrue((destination / "patches" / "TubularBell.inc").is_file())
            self.assertTrue((destination / "patches" / "README.md").is_file())
            self.assertTrue((destination / "docs" / "studio" / "csound" / "csound.js").is_file())
            self.assertTrue((destination / ".cursor" / "environment.json").is_file())
            self.assertIn("install.sh", (destination / ".cursor" / "environment.json").read_text())
            self.assertIn(">Home</a>", (destination / "docs" / "studio" / "index.html").read_text())
            self.assertNotIn("Michael Gogins</a>", (destination / "docs" / "studio" / "index.html").read_text())
            self.assertFalse((destination / "music" / "ai-compositions" / "nu").exists())
            self.assertFalse((destination / "music" / "ai-compositions" / "big_bell").exists())
            self.assertFalse((destination / "docs" / "compositions" / "nu").exists())
            names = {path.name for path in destination.iterdir()}
            self.assertNotIn("books", names)
            self.assertNotIn("writings", names)


if __name__ == "__main__":
    unittest.main()
