"""
HVAC system example for CEREBRUM framework.

This module implements a more complex HVAC system with multiple case-bearing
components (thermostat, heater, cooler) that interact through case relationships.
"""

import numpy as np
from typing import Dict, Any, List, Optional, Tuple, Union
import time
import sys

# Handle imports properly whether file is run directly or imported as a module
try:
    # Try relative imports (work when imported as a module)
    from ..core.model import Case, Model
    from ..core.active_inference import ActiveInferenceModel
except ImportError:
    # Fall back to absolute imports (work when run directly)
    from src.core.model import Case, Model
    from src.core.active_inference import ActiveInferenceModel

# Handle transformations imports
try:
    from ..transformations.case_transformations import (
        transform_case, 
        create_case_relationship,
        get_case_relationship,
        apply_morphosyntactic_alignment
    )
except ImportError:
    from src.transformations.case_transformations import (
        transform_case, 
        create_case_relationship,
        get_case_relationship,
        apply_morphosyntactic_alignment
    )
from src.examples.thermostat import ThermostatModel

class HeaterModel(ActiveInferenceModel):
    """
    A generative model of a heating system component.
    
    This model implements the heating functionality of an HVAC system
    and demonstrates case-bearing properties when interacting with
    other system components.
    """
    
    def __init__(
        self,
        name: str = "Heater",
        max_power: float = 5.0,
        efficiency: float = 0.9,
        response_time: float = 0.2
    ):
        """
        Initialize the heater model.
        
        Args:
            name: Model name
            max_power: Maximum heating power output
            efficiency: Efficiency of conversion from energy to heat
            response_time: How quickly the heater responds to control signals
        """
        # Set up the model with 2 states: power level and thermal output
        prior_means = np.array([0.0, 0.0])  # Default to off
        prior_precision = np.diag([1.0, 1.0])
        likelihood_precision = np.diag([10.0])
        
        super().__init__(
            name=name,
            parameters={
                "max_power": max_power,
                "efficiency": efficiency,
                "response_time": response_time,
                "min_power": 0.0
            },
            prior_means=prior_means,
            prior_precision=prior_precision,
            likelihood_precision=likelihood_precision
        )
        
        # Heater operational state
        self.current_power = 0.0  # Current power level
        self.current_output = 0.0  # Current heat output
        self.target_power = 0.0   # Target power level
        
        # History
        self.power_history = [self.current_power]
        self.output_history = [self.current_output]
        
        # Energy usage tracking
        self.energy_used = 0.0
        
        # State flags
        self.is_on = False
        self.is_faulty = False
        
        # Enhanced case configuration for heater
        self._setup_heater_case_configurations()
    
    def _setup_heater_case_configurations(self):
        """Set up heater-specific case configurations."""
        # NOMINATIVE case: actively generates heat
        self._case_configurations[Case.NOMINATIVE].update({
            "functions_enabled": ["generate_heat", "adjust_power"],
            "actuator_active": True,
            "parameter_focus": "output"
        })
        
        # ACCUSATIVE case: receives control updates
        self._case_configurations[Case.ACCUSATIVE].update({
            "functions_enabled": ["receive_power_setting"],
            "actuator_active": False,
            "parameter_focus": "control"
        })
        
        # GENITIVE case: generates thermal reports
        self._case_configurations[Case.GENITIVE].update({
            "functions_enabled": ["generate_thermal_report"],
            "actuator_active": False,
            "parameter_focus": "reporting"
        })
        
        # DATIVE case: receives temperature feedback
        self._case_configurations[Case.DATIVE].update({
            "functions_enabled": ["receive_feedback"],
            "actuator_active": False,
            "parameter_focus": "feedback"
        })
        
        # INSTRUMENTAL case: used as a heat source tool
        self._case_configurations[Case.INSTRUMENTAL].update({
            "functions_enabled": ["apply_heat"],
            "actuator_active": True,
            "parameter_focus": "execution"
        })
        
        # Additional cases configured with defaults
    
    def likelihood(self, states: np.ndarray) -> np.ndarray:
        """
        Map from hidden states to expected observations.
        
        For the heater, this maps from power and output states
        to the expected thermal contribution observation.
        
        Args:
            states: Hidden states of the model [power, output]
            
        Returns:
            Expected thermal contribution (heat output)
        """
        # The observable output is just the thermal output state
        return np.array([states[1]])
    
    def update_state(self, target_power: Optional[float] = None) -> Dict[str, float]:
        """
        Update the internal state of the heater based on target power.
        
        Args:
            target_power: Optional new target power setting
            
        Returns:
            Dictionary with updated state values
        """
        if target_power is not None:
            # Constrain target power to valid range
            self.target_power = max(self.parameters["min_power"], 
                                   min(self.parameters["max_power"], target_power))
        
        # Response time creates lag between target and actual power
        power_change = self.parameters["response_time"] * (self.target_power - self.current_power)
        self.current_power += power_change
        
        # Calculate thermal output based on power and efficiency
        self.current_output = self.current_power * self.parameters["efficiency"]
        
        # Track energy usage
        self.energy_used += self.current_power
        
        # Update history
        self.power_history.append(self.current_power)
        self.output_history.append(self.current_output)
        
        # Update on/off state
        self.is_on = self.current_power > 0.01
        
        return {
            "power": self.current_power,
            "output": self.current_output,
            "energy_used": self.energy_used,
            "is_on": self.is_on
        }
    
    # Case-specific update methods
    
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """
        Update for NOMINATIVE case: heater as active heat generator.
        
        Args:
            data: Optional power setting parameters
            
        Returns:
            Generation results
        """
        # Default behavior - continue with current target power
        target_power = None
        
        if isinstance(data, (int, float)):
            target_power = float(data)
        elif isinstance(data, dict) and "power" in data:
            target_power = float(data["power"])
        
        # Update heater state
        state = self.update_state(target_power)
        
        return {
            "status": "success",
            "power": state["power"],
            "output": state["output"],
            "is_on": state["is_on"],
            "case": "NOMINATIVE"
        }
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """
        Update for ACCUSATIVE case: heater receiving control signals.
        
        Args:
            data: Control signal data
            
        Returns:
            Update results
        """
        if data is None:
            return {"status": "error", "message": "Control signal required", "case": "ACCUSATIVE"}
        
        # Process power setting
        if isinstance(data, (int, float)):
            self.target_power = float(data)
        elif isinstance(data, dict) and "power" in data:
            self.target_power = float(data["power"])
        else:
            return {"status": "error", "message": "Invalid control format", "case": "ACCUSATIVE"}
        
        # Constrain to valid range
        self.target_power = max(self.parameters["min_power"], 
                               min(self.parameters["max_power"], self.target_power))
        
        # Record the change but don't immediately update state
        return {
            "status": "success",
            "target_power": self.target_power,
            "current_power": self.current_power,
            "case": "ACCUSATIVE"
        }
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """
        Update for GENITIVE case: heater generating operational reports.
        
        Args:
            data: Optional report parameters
            
        Returns:
            Generated report
        """
        # Generate heater performance report
        report = {
            "current_power": self.current_power,
            "target_power": self.target_power,
            "thermal_output": self.current_output,
            "is_on": self.is_on,
            "energy_used": self.energy_used,
            "efficiency": self.parameters["efficiency"]
        }
        
        # Add historical data if requested
        if isinstance(data, dict) and data.get("include_history", False):
            report["power_history"] = self.power_history
            report["output_history"] = self.output_history
        
        return {
            "status": "success",
            "report": report,
            "case": "GENITIVE"
        }
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """
        Update for DATIVE case: heater receiving temperature feedback.
        
        Args:
            data: Temperature feedback data
            
        Returns:
            Feedback reception results
        """
        if data is None:
            return {"status": "error", "message": "Feedback data required", "case": "DATIVE"}
        
        received_data = {}
        
        if isinstance(data, (int, float)):
            # Single temperature value
            received_data["temperature"] = float(data)
        elif isinstance(data, dict):
            received_data = data.copy()
        
        # Simply record the feedback, don't take action
        # (actions would be taken by the control system)
        
        return {
            "status": "success",
            "received_feedback": received_data,
            "case": "DATIVE"
        }
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """
        Update for INSTRUMENTAL case: heater used as a heating tool.
        
        Args:
            data: Action parameters
            
        Returns:
            Action results
        """
        if not isinstance(data, dict):
            return {"status": "error", "message": "Action parameters required", "case": "INSTRUMENTAL"}
        
        action = data.get("action", "apply_heat")
        
        if action == "apply_heat":
            # Apply heat at specified power
            power_level = data.get("power", self.current_power)
            if power_level == 0:
                return {
                    "status": "success",
                    "action": "apply_heat",
                    "result": "Heater is off",
                    "case": "INSTRUMENTAL"
                }
            
            # Set power and immediately update state
            state = self.update_state(power_level)
            
            return {
                "status": "success",
                "action": "apply_heat",
                "power": state["power"],
                "output": state["output"],
                "case": "INSTRUMENTAL"
            }
        
        elif action == "emergency_shutdown":
            # Emergency shutdown procedure
            self.target_power = 0.0
            self.current_power = 0.0
            self.current_output = 0.0
            self.is_on = False
            
            return {
                "status": "success",
                "action": "emergency_shutdown",
                "result": "Heater shut down",
                "case": "INSTRUMENTAL"
            }
        
        else:
            return {"status": "error", "message": f"Unknown action: {action}", "case": "INSTRUMENTAL"}


class CoolerModel(ActiveInferenceModel):
    """
    A generative model of a cooling system component.
    
    This model implements the cooling functionality of an HVAC system
    and demonstrates case-bearing properties when interacting with
    other system components.
    """
    
    def __init__(
        self,
        name: str = "Cooler",
        max_power: float = 5.0,
        efficiency: float = 0.8,
        response_time: float = 0.3
    ):
        """
        Initialize the cooler model.
        
        Args:
            name: Model name
            max_power: Maximum cooling power output
            efficiency: Efficiency of conversion from energy to cooling
            response_time: How quickly the cooler responds to control signals
        """
        # Set up the model with 2 states: power level and cooling output
        prior_means = np.array([0.0, 0.0])  # Default to off
        prior_precision = np.diag([1.0, 1.0])
        likelihood_precision = np.diag([10.0])
        
        super().__init__(
            name=name,
            parameters={
                "max_power": max_power,
                "efficiency": efficiency,
                "response_time": response_time,
                "min_power": 0.0
            },
            prior_means=prior_means,
            prior_precision=prior_precision,
            likelihood_precision=likelihood_precision
        )
        
        # Cooler operational state
        self.current_power = 0.0  # Current power level
        self.current_output = 0.0  # Current cooling output (negative for cooling)
        self.target_power = 0.0   # Target power level
        
        # History
        self.power_history = [self.current_power]
        self.output_history = [self.current_output]
        
        # Energy usage tracking
        self.energy_used = 0.0
        
        # State flags
        self.is_on = False
        self.is_faulty = False
        
        # Enhanced case configuration for cooler
        self._setup_cooler_case_configurations()
    
    def _setup_cooler_case_configurations(self):
        """Set up cooler-specific case configurations."""
        # NOMINATIVE case: actively generates cooling
        self._case_configurations[Case.NOMINATIVE].update({
            "functions_enabled": ["generate_cooling", "adjust_power"],
            "actuator_active": True,
            "parameter_focus": "output"
        })
        
        # ACCUSATIVE case: receives control updates
        self._case_configurations[Case.ACCUSATIVE].update({
            "functions_enabled": ["receive_power_setting"],
            "actuator_active": False,
            "parameter_focus": "control"
        })
        
        # GENITIVE case: generates cooling reports
        self._case_configurations[Case.GENITIVE].update({
            "functions_enabled": ["generate_cooling_report"],
            "actuator_active": False,
            "parameter_focus": "reporting"
        })
        
        # DATIVE case: receives temperature feedback
        self._case_configurations[Case.DATIVE].update({
            "functions_enabled": ["receive_feedback"],
            "actuator_active": False,
            "parameter_focus": "feedback"
        })
        
        # INSTRUMENTAL case: used as a cooling tool
        self._case_configurations[Case.INSTRUMENTAL].update({
            "functions_enabled": ["apply_cooling"],
            "actuator_active": True,
            "parameter_focus": "execution"
        })
        
        # Additional cases configured with defaults
    
    def likelihood(self, states: np.ndarray) -> np.ndarray:
        """
        Map from hidden states to expected observations.
        
        For the cooler, this maps from power and output states
        to the expected thermal contribution observation.
        
        Args:
            states: Hidden states of the model [power, output]
            
        Returns:
            Expected thermal contribution (cooling output - negative)
        """
        # The observable output is just the cooling output state (negative)
        return np.array([states[1]])
    
    def update_state(self, target_power: Optional[float] = None) -> Dict[str, float]:
        """
        Update the internal state of the cooler based on target power.
        
        Args:
            target_power: Optional new target power setting
            
        Returns:
            Dictionary with updated state values
        """
        if target_power is not None:
            # Constrain target power to valid range
            self.target_power = max(self.parameters["min_power"], 
                                   min(self.parameters["max_power"], target_power))
        
        # Response time creates lag between target and actual power
        power_change = self.parameters["response_time"] * (self.target_power - self.current_power)
        self.current_power += power_change
        
        # Calculate cooling output based on power and efficiency (negative for cooling)
        self.current_output = -self.current_power * self.parameters["efficiency"]
        
        # Track energy usage
        self.energy_used += self.current_power
        
        # Update history
        self.power_history.append(self.current_power)
        self.output_history.append(self.current_output)
        
        # Update on/off state
        self.is_on = self.current_power > 0.01
        
        return {
            "power": self.current_power,
            "output": self.current_output,
            "energy_used": self.energy_used,
            "is_on": self.is_on
        }
    
    # Case-specific update methods largely mirror the heater with appropriate changes
    
    def _update_nominative(self, data: Any) -> Dict[str, Any]:
        """
        Update for NOMINATIVE case: cooler as active cooling generator.
        
        Args:
            data: Optional power setting parameters
            
        Returns:
            Generation results
        """
        # Default behavior - continue with current target power
        target_power = None
        
        if isinstance(data, (int, float)):
            target_power = float(data)
        elif isinstance(data, dict) and "power" in data:
            target_power = float(data["power"])
        
        # Update cooler state
        state = self.update_state(target_power)
        
        return {
            "status": "success",
            "power": state["power"],
            "output": state["output"],
            "is_on": state["is_on"],
            "case": "NOMINATIVE"
        }
    
    def _update_accusative(self, data: Any) -> Dict[str, Any]:
        """
        Update for ACCUSATIVE case: cooler receiving control signals.
        
        Args:
            data: Control signal data
            
        Returns:
            Update results
        """
        if data is None:
            return {"status": "error", "message": "Control signal required", "case": "ACCUSATIVE"}
        
        # Process power setting
        if isinstance(data, (int, float)):
            self.target_power = float(data)
        elif isinstance(data, dict) and "power" in data:
            self.target_power = float(data["power"])
        else:
            return {"status": "error", "message": "Invalid control format", "case": "ACCUSATIVE"}
        
        # Constrain to valid range
        self.target_power = max(self.parameters["min_power"], 
                               min(self.parameters["max_power"], self.target_power))
        
        # Record the change but don't immediately update state
        return {
            "status": "success",
            "target_power": self.target_power,
            "current_power": self.current_power,
            "case": "ACCUSATIVE"
        }
    
    def _update_genitive(self, data: Any) -> Dict[str, Any]:
        """
        Update for GENITIVE case: cooler generating operational reports.
        
        Args:
            data: Optional report parameters
            
        Returns:
            Generated report
        """
        # Generate cooler performance report
        report = {
            "current_power": self.current_power,
            "target_power": self.target_power,
            "cooling_output": self.current_output,
            "is_on": self.is_on,
            "energy_used": self.energy_used,
            "efficiency": self.parameters["efficiency"]
        }
        
        # Add historical data if requested
        if isinstance(data, dict) and data.get("include_history", False):
            report["power_history"] = self.power_history
            report["output_history"] = self.output_history
        
        return {
            "status": "success",
            "report": report,
            "case": "GENITIVE"
        }
    
    def _update_dative(self, data: Any) -> Dict[str, Any]:
        """Update for DATIVE case: cooler receiving temperature feedback."""
        if data is None:
            return {"status": "error", "message": "Feedback data required", "case": "DATIVE"}
        
        received_data = {}
        
        if isinstance(data, (int, float)):
            # Single temperature value
            received_data["temperature"] = float(data)
        elif isinstance(data, dict):
            received_data = data.copy()
        
        # Simply record the feedback, don't take action
        
        return {
            "status": "success",
            "received_feedback": received_data,
            "case": "DATIVE"
        }
    
    def _update_instrumental(self, data: Any) -> Dict[str, Any]:
        """Update for INSTRUMENTAL case: cooler used as a cooling tool."""
        if not isinstance(data, dict):
            return {"status": "error", "message": "Action parameters required", "case": "INSTRUMENTAL"}
        
        action = data.get("action", "apply_cooling")
        
        if action == "apply_cooling":
            # Apply cooling at specified power
            power_level = data.get("power", self.current_power)
            if power_level == 0:
                return {
                    "status": "success",
                    "action": "apply_cooling",
                    "result": "Cooler is off",
                    "case": "INSTRUMENTAL"
                }
            
            # Set power and immediately update state
            state = self.update_state(power_level)
            
            return {
                "status": "success",
                "action": "apply_cooling",
                "power": state["power"],
                "output": state["output"],
                "case": "INSTRUMENTAL"
            }
        
        elif action == "emergency_shutdown":
            # Emergency shutdown procedure
            self.target_power = 0.0
            self.current_power = 0.0
            self.current_output = 0.0
            self.is_on = False
            
            return {
                "status": "success",
                "action": "emergency_shutdown",
                "result": "Cooler shut down",
                "case": "INSTRUMENTAL"
            }
        
        else:
            return {"status": "error", "message": f"Unknown action: {action}", "case": "INSTRUMENTAL"}


class HVACSystem:
    """
    An integrated HVAC system with thermostat, heater, and cooler.
    
    This class demonstrates how multiple case-bearing models can interact
    through case relationships.
    """
    
    def __init__(
        self,
        name: str = "HVAC System",
        target_temperature: float = 22.0,
        temperature_precision: float = 10.0,
        heater_power: float = 5.0,
        cooler_power: float = 5.0
    ):
        """
        Initialize the HVAC system with thermostat, heater, and cooler.
        
        Args:
            name: System name
            target_temperature: Initial target temperature
            temperature_precision: Precision of temperature control
            heater_power: Maximum heater power
            cooler_power: Maximum cooler power
        """
        self.name = name
        
        # Create component models
        self.thermostat = ThermostatModel(
            name=f"{name}_Thermostat",
            target_temperature=target_temperature,
            temperature_precision=temperature_precision
        )
        
        self.heater = HeaterModel(
            name=f"{name}_Heater",
            max_power=heater_power
        )
        
        self.cooler = CoolerModel(
            name=f"{name}_Cooler",
            max_power=cooler_power
        )
        
        # System state
        self.is_on = False
        self.mode = "auto"  # auto, heat, cool, off
        self.current_temperature = target_temperature
        
        # Components are initially nominative
        transform_case(self.thermostat, Case.NOMINATIVE)
        transform_case(self.heater, Case.NOMINATIVE)
        transform_case(self.cooler, Case.NOMINATIVE)
    
    def setup_relationships(self):
        """
        Set up case relationships between system components.
        
        When components interact, they adopt appropriate cases
        based on their functional roles in the relationship.
        """
        # Thermostat controls heater and cooler
        create_case_relationship(self.thermostat, self.heater, "controls")
        create_case_relationship(self.thermostat, self.cooler, "controls")
        
        # Heater and cooler provide temperature effects
        create_case_relationship(self.heater, self.thermostat, "provides_heat")
        create_case_relationship(self.cooler, self.thermostat, "provides_cooling")
        
        return {
            "thermostat_case": self.thermostat.case,
            "heater_case": self.heater.case,
            "cooler_case": self.cooler.case
        }
    
    def align_components_for_action(self, action: str):
        """
        Align component cases for a specific system action.
        
        Args:
            action: The action to perform ("heat", "cool", "monitor", "report")
            
        Returns:
            Dictionary of case assignments
        """
        if action == "heat":
            # Thermostat commands heater, cooler is idle
            transform_case(self.thermostat, Case.NOMINATIVE)  # Controller
            transform_case(self.heater, Case.ACCUSATIVE)      # Receiver of commands
            transform_case(self.cooler, Case.ABLATIVE)        # Background reference
        
        elif action == "cool":
            # Thermostat commands cooler, heater is idle
            transform_case(self.thermostat, Case.NOMINATIVE)  # Controller
            transform_case(self.cooler, Case.ACCUSATIVE)      # Receiver of commands
            transform_case(self.heater, Case.ABLATIVE)        # Background reference
        
        elif action == "monitor":
            # All components monitor current state
            transform_case(self.thermostat, Case.LOCATIVE)    # Environmental context
            transform_case(self.heater, Case.DATIVE)          # Receiving data
            transform_case(self.cooler, Case.DATIVE)          # Receiving data
        
        elif action == "report":
            # All components generate reports
            transform_case(self.thermostat, Case.GENITIVE)    # Generating report
            transform_case(self.heater, Case.GENITIVE)        # Generating report
            transform_case(self.cooler, Case.GENITIVE)        # Generating report
        
        else:
            # Default alignment
            transform_case(self.thermostat, Case.NOMINATIVE)
            transform_case(self.heater, Case.NOMINATIVE)
            transform_case(self.cooler, Case.NOMINATIVE)
        
        return {
            "thermostat_case": self.thermostat.case,
            "heater_case": self.heater.case,
            "cooler_case": self.cooler.case,
            "action": action
        }
    
    def update(self, steps: int = 1) -> Dict[str, Any]:
        """
        Update the HVAC system for a number of steps.
        
        This demonstrates how the components interact through
        case-based transformations.
        
        Args:
            steps: Number of simulation steps to run
            
        Returns:
            Results of the simulation
        """
        results = {
            "temperature": [],
            "heater_power": [],
            "cooler_power": [],
            "actions": []
        }
        
        for step in range(steps):
            # 1. Thermostat senses temperature
            transform_case(self.thermostat, Case.NOMINATIVE)
            thermostat_result = self.thermostat.update()
            temp_error = thermostat_result["error"]
            self.current_temperature = thermostat_result["temperature"]
            
            # 2. Determine if heating or cooling is needed
            action = None
            if self.mode == "auto":
                if temp_error > 0.5:  # Too cold
                    action = "heat"
                elif temp_error < -0.5:  # Too hot
                    action = "cool"
                else:  # Within acceptable range
                    action = "maintain"
            elif self.mode == "heat":
                action = "heat"
            elif self.mode == "cool":
                action = "cool"
            else:  # mode == "off"
                action = "off"
            
            # 3. Apply appropriate case transformations
            self.align_components_for_action(action)
            
            # 4. Execute the action
            heater_power = 0.0
            cooler_power = 0.0
            
            if action == "heat":
                # Scale heating power based on temperature error
                heater_power = min(self.heater.parameters["max_power"], max(0.5, abs(temp_error)))
                self.heater.update({"power": heater_power})
                self.cooler.update({"power": 0.0})
            
            elif action == "cool":
                # Scale cooling power based on temperature error
                cooler_power = min(self.cooler.parameters["max_power"], max(0.5, abs(temp_error)))
                self.cooler.update({"power": cooler_power})
                self.heater.update({"power": 0.0})
            
            elif action == "maintain" or action == "off":
                # Turn off both heater and cooler
                self.heater.update({"power": 0.0})
                self.cooler.update({"power": 0.0})
            
            # 5. Apply thermal effects to room
            # Set thermostat to DATIVE to receive temperature updates
            transform_case(self.thermostat, Case.DATIVE)
            
            # Calculate combined thermal effect
            heater_output = self.heater.current_output
            cooler_output = self.cooler.current_output
            combined_effect = heater_output + cooler_output
            
            # Update room temperature with thermal effect
            new_temp = self.current_temperature + combined_effect
            
            # Apply to thermostat
            self.thermostat.update({"temperature": new_temp})
            self.current_temperature = new_temp
            
            # 6. Record results
            results["temperature"].append(self.current_temperature)
            results["heater_power"].append(self.heater.current_power)
            results["cooler_power"].append(self.cooler.current_power)
            results["actions"].append(action)
        
        return results
    
    def generate_system_report(self) -> Dict[str, Any]:
        """
        Generate a comprehensive report of system status.
        
        Sets all components to GENITIVE case to generate reports.
        
        Returns:
            System report
        """
        self.align_components_for_action("report")
        
        thermostat_report = self.thermostat.update({"report_type": "standard"})
        heater_report = self.heater.update({"include_history": True})
        cooler_report = self.cooler.update({"include_history": True})
        
        return {
            "system_name": self.name,
            "mode": self.mode,
            "is_on": self.is_on,
            "current_temperature": self.current_temperature,
            "thermostat": thermostat_report.get("report", {}),
            "heater": heater_report.get("report", {}),
            "cooler": cooler_report.get("report", {})
        }
    
    def set_mode(self, mode: str) -> Dict[str, Any]:
        """
        Set the operation mode of the HVAC system.
        
        Args:
            mode: Operation mode ("auto", "heat", "cool", "off")
            
        Returns:
            Status of the mode change
        """
        if mode not in ["auto", "heat", "cool", "off"]:
            return {"status": "error", "message": f"Invalid mode: {mode}"}
        
        self.mode = mode
        
        # Update component states based on new mode
        if mode == "off":
            self.is_on = False
            transform_case(self.thermostat, Case.ABLATIVE)  # Historical reference only
            transform_case(self.heater, Case.INSTRUMENTAL)  # Tool mode for shutdown
            transform_case(self.cooler, Case.INSTRUMENTAL)  # Tool mode for shutdown
            
            # Shutdown components
            self.heater.update({"action": "emergency_shutdown"})
            self.cooler.update({"action": "emergency_shutdown"})
        else:
            self.is_on = True
            transform_case(self.thermostat, Case.NOMINATIVE)  # Active control
            transform_case(self.heater, Case.ACCUSATIVE)      # Ready to receive commands
            transform_case(self.cooler, Case.ACCUSATIVE)      # Ready to receive commands
        
        return {
            "status": "success",
            "mode": self.mode,
            "is_on": self.is_on,
            "thermostat_case": self.thermostat.case.name,
            "heater_case": self.heater.case.name,
            "cooler_case": self.cooler.case.name
        }
    
    def simulate_scenario(
        self,
        steps: int,
        ambient_temperatures: Optional[List[float]] = None,
        target_changes: Optional[Dict[int, float]] = None,
        mode_changes: Optional[Dict[int, str]] = None
    ) -> Dict[str, Any]:
        """
        Simulate an HVAC scenario over multiple timesteps.
        
        Args:
            steps: Number of steps to simulate
            ambient_temperatures: Optional list of ambient temps for each step
            target_changes: Optional dict mapping steps to new target temperatures
            mode_changes: Optional dict mapping steps to mode changes
            
        Returns:
            Dictionary with simulation results
        """
        results = {
            "temperature": [],
            "target": [],
            "ambient": [],
            "heater_power": [],
            "cooler_power": [],
            "actions": [],
            "modes": []
        }
        
        for step in range(steps):
            # Apply ambient temperature changes if specified
            if ambient_temperatures and step < len(ambient_temperatures):
                transform_case(self.thermostat, Case.LOCATIVE)
                self.thermostat.update({"ambient_temperature": ambient_temperatures[step]})
                results["ambient"].append(ambient_temperatures[step])
            else:
                results["ambient"].append(self.thermostat.ambient_temperature)
            
            # Apply target temperature changes if specified
            if target_changes and step in target_changes:
                transform_case(self.thermostat, Case.ACCUSATIVE)
                self.thermostat.update({"target_temperature": target_changes[step]})
            
            # Apply mode changes if specified
            if mode_changes and step in mode_changes:
                self.set_mode(mode_changes[step])
            
            # Run one update step
            step_result = self.update(1)
            
            # Record results
            results["temperature"].append(step_result["temperature"][-1])
            results["heater_power"].append(step_result["heater_power"][-1])
            results["cooler_power"].append(step_result["cooler_power"][-1])
            results["actions"].append(step_result["actions"][-1])
            results["target"].append(self.thermostat.parameters["target_temperature"])
            results["modes"].append(self.mode)
        
        return results


def demonstrate_hvac_system():
    """
    Demonstrate the HVAC system with case transformations.
    
    This function creates an HVAC system and demonstrates
    how the components interact through case relationships.
    
    Returns:
        Dictionary with demonstration results
    """
    # Create HVAC system
    hvac = HVACSystem(
        name="Demo HVAC",
        target_temperature=22.0,
        heater_power=3.0,
        cooler_power=2.5
    )
    
    # Set up case relationships
    relationships = hvac.setup_relationships()
    
    # Run a demonstration scenario
    scenario_results = hvac.simulate_scenario(
        steps=20,
        ambient_temperatures=[15.0] * 10 + [30.0] * 10,  # Cold then hot
        target_changes={5: 23.0, 15: 21.0},              # Change target temps
        mode_changes={0: "auto", 10: "cool"}             # Change modes
    )
    
    # Generate system report
    report = hvac.generate_system_report()
    
    return {
        "relationships": relationships,
        "scenario": scenario_results,
        "report": report
    }


if __name__ == "__main__":
    # Demonstrate the HVAC system
    results = demonstrate_hvac_system()
    
    # Print scenario results
    print("\n=== HVAC System Case-Based Operation ===")
    print(f"Final temperature: {results['scenario']['temperature'][-1]}")
    print(f"Final mode: {results['scenario']['modes'][-1]}")
    print(f"Total heater energy: {sum(results['scenario']['heater_power'])}")
    print(f"Total cooler energy: {sum(results['scenario']['cooler_power'])}")
    
    # Print component case relationships
    print("\n=== Component Case Relationships ===")
    print(results["relationships"]) 