using System.Linq;
using Constructor.Ships;
using Constructor.Ships.Modification;
using GameDatabase;
using GameDatabase.DataModel;
using GameDatabase.Enums;
using Services.Localization;
using Services.Resources;
using UnityEngine;
using UnityEngine.UI;

namespace ShipEditor.UI
{
    public class ShipItem : MonoBehaviour
    {
        [SerializeField] private Image _icon;
        [SerializeField] private Text _name;
        
        [SerializeField] private Text _classLabel;
		[SerializeField] private Text _classText;
		
		[SerializeField] private Text _levelLabel;
		[SerializeField] private Text _levelText;

		[SerializeField] private GameObject _modificationField;
		[SerializeField] private Image _perk1icon;
		[SerializeField] private Image _perk2icon;
	    [SerializeField] private Image _perk3icon;
 		
	    [SerializeField] private GameObject _moreInfoField;
		[SerializeField] private Image _playerIcon;
		[SerializeField] private Image _botIcon;
		[SerializeField] private Image _diffcultyIcon;
		[SerializeField] private Text _difficultyText;
		[SerializeField] private Image _expansionIcon;
		
		public void Initialize(IShip ship, IResourceLocator resourceLocator, ILocalization localization, IDatabase database)
        {
            Ship = ship;
            var icon = resourceLocator.GetSprite(ship.Model.IconImage);
            if (icon)
            {
                _icon.sprite = icon;
                _icon.rectTransform.localScale = 1.4f * ship.Model.IconScale * Vector3.one;
            }
            else
            {
                _icon.sprite = resourceLocator.GetSprite(ship.Model.ModelImage);
                _icon.rectTransform.localScale = Vector3.one;
            }

            _icon.color = ship.ColorScheme.HsvColor.ToUnity();

			_name.text = localization.GetString(ship.Name);
			_classText.text = ship.Model.SizeClass.ToString(localization);
			
			if (!database.IsEditable) {
				_modificationField.SetActive(false);
				_moreInfoField.SetActive(false);
				
				var level = ship.Experience.Level;
				_levelText.text = level > 0 ? level.ToString() : "0";
			}
			
			// Modifications List 
			var modifications = ship.Model.Modifications;
			var perkIcons = new[] { _perk1icon, _perk2icon, _perk3icon };

			for (int i = 0; i < perkIcons.Length; i++)
			{
				if (i < modifications.Count)
				{
					perkIcons[i].sprite = resourceLocator.GetSprite(modifications[i].Type.GetIconId());
					perkIcons[i].enabled = true;
				}
				else
				{
					perkIcons[i].sprite = null;
					perkIcons[i].enabled = false;
				}
			}

			if (database.IsEditable)
			{	
				_levelLabel.enabled = false;
				_levelText.enabled = false;
				
				var isAvailableForPlayer = ShipValidator.IsAvailableForPlayer(Ship);
				_playerIcon.color = isAvailableForPlayer ? Color.green : Color.red;

				var originalShip = Ship.Model.OriginalShip;
				var shipBuild = database.ShipBuildList.FirstOrDefault(b => b.Ship == originalShip);
				var isAvailableForEnemy = shipBuild?.AvailableForEnemy ?? true;
				_botIcon.color = isAvailableForEnemy ? Color.green : Color.red;

				var isExpansionDisabled = LayoutModifications.IsDisabledForShip(ship.Model.OriginalShip, database.ShipSettings);
				_expansionIcon.color = isExpansionDisabled ? Color.red : Color.green;
				
				_difficultyText.text = ship.ExtraThreatLevel switch
				{
					DifficultyClass.Default => "0",
					DifficultyClass.Class1 => "1vet",
					DifficultyClass.Class2 => "2vet",
					DifficultyClass.Class3 => "3vet",
					_ => "Unknown"
				};
			}
		}

		public IShip Ship { get; private set; }
    }
}
